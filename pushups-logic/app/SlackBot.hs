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

import PushupsCommander
import Cob
import ChatBot

-------- Chat Bot -----------

instance ChatBotMessage (SlackEventWrapper Message) where
    getServerId = team_id
    getUserId = user . event
    getText = text . event

instance MonadIO m => Chattable m (SlackToken, CobSession) (SlackEventWrapper Message) where
    reactTo reader (SlackEventWrapper (Message chan ts _ _) _) text = liftIO $
        postMessage reader (object
                [ "channel"   .= chan
                , "name"      .= text
                , "timestamp" .= ts ])
             "reactions.add"
    replyTo reader (SlackEventWrapper (Message chan _ _ _) _) text = liftIO $
        postMessage reader (object
                [ "channel" .= chan
                , "text"    .= text ])
            "chat.postMessage"

postMessage :: (SlackToken, CobSession) -> Value -> ByteString -> IO ()
postMessage (slackToken, session) message method = do
    let request = setRequestBodyJSON message $
                  addRequestHeader "Authorization" ("Bearer " <> encodeUtf8 slackToken) $
                  setRequestManager (tlsmanager session) $
                    Net.defaultRequest
                        { secure = True
                        , port = 443
                        , method = "POST"
                        , host   = "slack.com"
                        , path   = "/api/" <> method }
    response <- httpNoBody request
    print response

-------- Bot API ------------

type SlackEvents = "slack" :> ReqBody '[JSON] SlackEvent :> Post '[JSON] Text

slackHandler :: Bot Handler (SlackToken, CobSession) (SlackEventWrapper Message) () -> (SlackToken, CobSession) -> Server SlackEvents
slackHandler bot (slackToken, session) = \case
    UrlVerification x -> pure (challenge x)
    WrappedEvent m@(SlackEventWrapper Message{} _) ->
        runBot bot m (slackToken, session) $> ""

type SlackServer = BotServer SlackEvents (SlackToken, CobSession) (SlackEventWrapper Message) ()

slackBot :: BotServerConstructor SlackServer
slackBot = BotServer (Proxy @SlackEvents) slackHandler

-------- API Types ----------

type SlackToken = Text

data UrlVerification' = UrlVerification' { token :: Text, challenge :: Text } deriving (Show, Generic)

data SlackEventWrapper a = SlackEventWrapper { event :: a, team_id :: Text } deriving (Show, Generic)

data Message = Message { channel :: Text
                       , ts      :: Text
                       , user    :: Text
                       , text    :: Text
                       } deriving (Show)

data SlackEvent = UrlVerification UrlVerification'
                | WrappedEvent (SlackEventWrapper Message)

instance FromJSON UrlVerification'

instance FromJSON a => FromJSON (SlackEventWrapper a)

instance FromJSON Message where
     parseJSON = withObject "message" $ \v -> do
         "message" :: Text <- v .: "type"
         channel <- v .: "channel"
         user <- v .: "user"
         text <- v .: "text"
         ts <- v .: "ts"
         return (Message channel ts user text)

instance FromJSON SlackEvent where
    parseJSON = withObject "slackEvent" $ \v -> do
        ty :: Text <- v .: "type"
        case ty of
          "event_callback"   -> WrappedEvent <$> parseJSON (Object v)
          "url_verification" -> UrlVerification <$> parseJSON (Object v)
          _ -> fail $ unpack $ "slack event type (" <> ty <> ") not supported"

