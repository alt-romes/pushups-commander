{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Text.IO as TIO
import Data.Text as T
import Data.ByteString (ByteString(..))
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Reader
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Data.Aeson
import Servant
import Servant.API
import Servant.API.ContentTypes

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), defaultRequest, newManager, tlsManagerSettings)
import Network.HTTP.Simple hiding (Proxy)

import PushupsCommander
import Cob
import ChatBot

type SlackToken = Text

data UrlVerification' = UrlVerification' { token :: Text, challenge :: Text } deriving (Show, Generic)
instance FromJSON UrlVerification'

data SlackEventWrapper a = SlackEventWrapper { event :: a, team_id :: Text } deriving (Show, Generic)
instance FromJSON a => FromJSON (SlackEventWrapper a)

data Message = Message { channel :: Text
                       , ts      :: Text
                       , user    :: Text
                       , text    :: Text
                       } deriving (Show)

data SlackEvent = UrlVerification UrlVerification'
                | WrappedEvent (SlackEventWrapper Message)

type SlackEvents = "slack" :> ReqBody '[JSON] SlackEvent :> Post '[JSON] Text

slackHandler :: SlackToken -> CobSession -> Server SlackEvents
slackHandler slackToken session = slackEvent
    where
    slackEvent :: SlackEvent -> Handler Text
    slackEvent = \case
        UrlVerification x -> urlVerification x
        WrappedEvent m@(SlackEventWrapper Message{} _) -> message m

    urlVerification :: UrlVerification' -> Handler Text
    urlVerification v = do
        liftIO $ print v
        return $ challenge v

    message :: SlackEventWrapper Message -> Handler Text
    message m = liftIO (runChat (slackToken, session) (handleMsg m)) >> return ""

type instance ReplyMessageState (SlackEventWrapper Message) = (SlackToken, CobSession)

instance ChatBotMessage (SlackEventWrapper Message) where
    getServerId = team_id
    getUserId = user . event
    getText = text . event
    reactTo (SlackEventWrapper (Message chan ts _ _) _) text = do
        (slackToken, session) <- ask
        lift $ postMessage slackToken session 
            (object
                [ "channel"   .= chan
                , "name"      .= text
                , "timestamp" .= ts ])
             "reactions.add"
    replyTo (SlackEventWrapper (Message chan _ _ _) _) text = do
        (slackToken, session) <- ask
        lift $ postMessage slackToken session
            (object
                [ "channel" .= chan
                , "text"    .= text ])
            "chat.postMessage"
    handleMsg message = do
        (slackToken, session) <- ask
        runCobT session (commandHandler message) >>= either (replyTo message . pack) return


postMessage :: SlackToken -> CobSession -> Value -> ByteString -> IO ()
postMessage slackToken session message method = do
    let request = setRequestBodyJSON message $
                  addRequestHeader "Authorization" ("Bearer " <> encodeUtf8 slackToken) $
                    defRequest
                      { method = "POST"
                      , host   = "slack.com"
                      , path   = "/api/" <> method }
    response <- httpNoBody request
    print response
    where
        defRequest = setRequestManager (tlsmanager session) Net.defaultRequest { secure = True, port = 443 }

main :: IO ()
main = do
    slackToken  <- T.init <$> TIO.readFile "slack-token.secret"
    host     <- Prelude.init <$> readFile "cob-host.secret"
    cobtoken <- Prelude.init <$> readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    print "Starting..."
    runChatBots [(ChatBot @SlackEvents (slackHandler slackToken session), 25564)]


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
