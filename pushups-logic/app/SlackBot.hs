{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module SlackBot where

import Crypto.Hash (digestFromByteString)
import Crypto.MAC.HMAC (hmac, HMAC(..))
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.Text.IO as TIO
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Functor.Identity (Identity(..))
import Data.Text as T
import qualified Data.Text.Lazy as LT (Text)
import Data.ByteString (ByteString(..))
import qualified Data.ByteString.Lazy as LB (ByteString(..), toChunks)
import qualified Data.ByteString.Char8 as B8 (concat)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)

import Control.Monad.Reader
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Data.Functor (($>))
import Data.Aeson
import Servant

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), defaultRequest, newManager, tlsManagerSettings)
import Network.HTTP.Simple (setRequestBodyJSON, addRequestHeader, setRequestManager, httpNoBody)

import Cob
import PushupsCommander
import Bots

-------- Chat Bot -----------

instance ChatBotMessage (SlackEventWrapper Message) where
    getServerId = team_id
    getUserId   = user . event
    getContent  = text . event
    isFromBot   = isJust . botId . event

instance Chattable SlackHandler (SlackEventWrapper Message) where
    reactTo (SlackEventWrapper (Message chan ts _ _ _) _) text =
        postMessage (object
                [ "channel"   .= chan
                , "name"      .= text
                , "timestamp" .= ts ])
             "reactions.add"
    replyTo (SlackEventWrapper (Message chan _ _ _ _) _) text =
        postMessage (object
                [ "channel" .= chan
                , "text"    .= text ])
            "chat.postMessage"

postMessage :: Value -> ByteString -> SlackHandler ()
postMessage message method = do
    (botToken, _, session) <- ask
    let request = setRequestBodyJSON message $
                  addRequestHeader "Authorization" ("Bearer " <> encodeUtf8 botToken) $
                  setRequestManager (tlsmanager session) $
                    Net.defaultRequest
                        { secure = True
                        , port = 443
                        , method = "POST"
                        , host   = "slack.com"
                        , path   = "/api/" <> method }
    response <- httpNoBody request
    liftIO (print response)

-------- Bot API ------------

instance {-# OVERLAPPING #-} MimeUnrender JSON LB.ByteString where
    mimeUnrender _ = Right

instance FromHttpApiData (HMAC SHA256) where
    parseQueryParam = maybe (Left "Signature is cryptographically incorrect.") (Right . HMAC) . digestFromByteString . encodeUtf8
    parseHeader = maybe (Left "Signature is cryptographically incorrect.") (Right . HMAC) . digestFromByteString

type SlackEvents = "slack"
                 :> Header "X-Slack-Request-Timestamp" Text
                 -- :> Header "X-Slack-Signature" (HMAC SHA256)
                 :> Header "X-Slack-Signature" String
                 :> ReqBody '[JSON] LB.ByteString
                 :> Post '[JSON] Text

type SlackHandler = ReaderT (BotToken, BotToken, CobSession) Handler

slackHandler :: Bot SlackHandler (SlackEventWrapper Message) [ChatBotCommand] -> ServerT SlackEvents SlackHandler
slackHandler bot (Just timestamp) (Just slackSignature) rawbody = do
    signingSecret <- asks (\(_,s,_) -> encodeUtf8 s)
    let localSignature = "v0=" <> show (hmacGetDigest (hmac signingSecret ("v0:" <> encodeUtf8 timestamp <> ":" <> lazyToStrict rawbody) :: HMAC SHA256))
    if localSignature /= slackSignature
      then pure "Signatures do not match!"
      else case decode @SlackEvent rawbody of
        Nothing -> pure "Couldn't decode a slack event from the body"
        Just slackEvent -> case slackEvent of
          UrlVerification x -> pure (challenge x)
          WrappedEvent m@(SlackEventWrapper Message{} _) -> runChatBot bot m $> ""
    where
        lazyToStrict = B8.concat . LB.toChunks
slackHandler bot _ _ rawbody = pure "Required request headers (X-Slack-...) not provided"

slackBot :: (BotToken, BotToken, CobSession) -> Bot Identity (Bot SlackHandler (SlackEventWrapper Message) [ChatBotCommand]) Application
slackBot r = Bot (pure . serve (Proxy @SlackEvents) . hoistServer (Proxy @SlackEvents) (`runReaderT` r) . slackHandler)

slackServer :: (BotToken, BotToken, CobSession) -> ChatBotServer
slackServer = ChatBotServer . slackBot

-------- API Types ----------

data UrlVerification' = UrlVerification' { token :: Text, challenge :: Text } deriving (Show, Generic)

data SlackEventWrapper a = SlackEventWrapper { event :: a, team_id :: Text } deriving (Show, Generic)

data Message = Message { channel :: Text
                       , ts      :: Text
                       , user    :: Text
                       , text    :: Text
                       , botId   :: Maybe Text
                       } deriving (Show)

data SlackEvent = UrlVerification UrlVerification'
                | WrappedEvent (SlackEventWrapper Message)

instance FromJSON UrlVerification'

instance FromJSON a => FromJSON (SlackEventWrapper a)

instance FromJSON Message where
     parseJSON = withObject "message" $ \v -> do
         "message" :: Text <- v .: "type"
         maybeBotId <- v .:? "bot_id"
         channel <- v .: "channel"
         user <- v .: "user"
         text <- v .: "text"
         ts <- v .: "ts"
         return (Message channel ts user text maybeBotId)

instance FromJSON SlackEvent where
    parseJSON = withObject "slackEvent" $ \v -> do
        ty :: Text <- v .: "type"
        case ty of
          "event_callback"   -> WrappedEvent <$> parseJSON (Object v)
          "url_verification" -> UrlVerification <$> parseJSON (Object v)
          _ -> fail $ unpack $ "slack event type (" <> ty <> ") not supported"

