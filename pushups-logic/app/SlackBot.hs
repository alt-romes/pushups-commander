{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module SlackBot where

import qualified Data.Text.IO as TIO
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Functor.Identity (Identity(..))
import Data.Text as T
import Data.ByteString (ByteString(..))
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Reader
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Data.Functor (($>))
import Data.Aeson
import Servant
import Servant.API
import Servant.API.ContentTypes

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), defaultRequest, newManager, tlsManagerSettings)
import Network.HTTP.Simple hiding (Proxy)

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
    (botToken, session) <- ask
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

type SlackEvents = "slack" :> ReqBody '[JSON] SlackEvent :> Post '[JSON] Text

type SlackHandler = ReaderT (BotToken, CobSession) Handler

slackHandler :: ChatBot SlackHandler (SlackEventWrapper Message) -> ServerT SlackEvents SlackHandler
slackHandler bot = \case
    UrlVerification x -> pure (challenge x)
    WrappedEvent m@(SlackEventWrapper Message{} _) -> runChatBot bot m $> ""

slackBot :: (BotToken, CobSession) -> Bot Identity (ChatBot SlackHandler (SlackEventWrapper Message)) Application
slackBot r = Bot (pure . serve (Proxy @SlackEvents) . hoistServer (Proxy @SlackEvents) (`runReaderT` r) . slackHandler)


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

