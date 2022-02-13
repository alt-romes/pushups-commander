{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Text.IO as TIO
import Data.Text as T

import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Data.Aeson
import Servant
import Servant.API
import Servant.API.ContentTypes
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

import PushupsCommander

data SlackEventWrapper a = SlackEventWrapper
    { event   :: a
    , team_id :: Text
    } deriving (Show, Generic)
instance FromJSON a => FromJSON (SlackEventWrapper a)

data Message = Message
    { channel :: Text
    , user    :: Text
    , text    :: Text
    } deriving (Show)
instance FromJSON Message where
     parseJSON = withObject "message" $ \v -> do
         "message" :: Text <- v .: "type"
         channel <- v .: "channel"
         user <- v .: "user"
         text <- v .: "text"
         return (Message channel user text)

data SlackEvent = UrlVerification UrlVerification'
                | WrappedEvent (SlackEventWrapper Message)

instance FromJSON SlackEvent where
    parseJSON = withObject "slackEvent" $ \v -> do
        ty :: Text <- v .: "type"
        case ty of
          "event_callback"   -> WrappedEvent <$> parseJSON (Object v)
          "url_verification" -> UrlVerification <$> parseJSON (Object v)
          _ -> fail $ unpack $ "slack event type (" <> ty <> ") not supported"


type SlackEvents = "slack" :> ReqBody '[JSON] SlackEvent :> Post '[JSON] Text

slackEventsAPI :: Proxy SlackEvents
slackEventsAPI = Proxy

handler :: Server SlackEvents
handler = event
            
    where
    event :: SlackEvent -> Handler Text
    event = \case
        UrlVerification x -> urlVerification x
        WrappedEvent m@(SlackEventWrapper Message{} _) -> message m

    urlVerification :: UrlVerification' -> Handler Text
    urlVerification v = do
        liftIO $ print v
        return $ _challenge v

    message :: SlackEventWrapper Message -> Handler Text
    message = (<$>) (const "") . liftIO . print


app :: Application
app = serve slackEventsAPI handler


main :: IO ()
main = do
    -- slackToken  <- T.init <$> TIO.readFile "slack-token.secret"
    print "Starting..."
    run 25564 (logStdoutDev app)


data UrlVerification' = UrlVerification'
    { _token     :: Text
    , _challenge :: Text }
    deriving (Show)
instance FromJSON UrlVerification' where
     parseJSON = withObject "url_verification" $ \v -> do
         tok <- v .: "token"
         cha <- v .: "challenge"
         return (UrlVerification' tok cha)
