{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Text.IO as TIO
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)

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

import Network.HTTP.Conduit as Net (Manager, Cookie(..), Request(..), defaultRequest, newManager, tlsManagerSettings)
import Network.HTTP.Simple hiding (Proxy)

import PushupsCommander
import Cob

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
instance ToJSON Message where
    toJSON (Message channel _ text) = object
        [ "channel" .= channel
        , "text"    .= text ]


type SlackToken = Text

getServerId :: SlackEventWrapper Message -> Text
getServerId = team_id

getUserId :: SlackEventWrapper Message -> Text
getUserId = user . event

createReaction :: SlackToken -> CobSession -> SlackEventWrapper Message -> Text -> IO ()
createReaction = replyToMsg

replyToMsg :: SlackToken -> CobSession -> SlackEventWrapper Message -> Text -> IO ()
replyToMsg slackToken session (SlackEventWrapper (Message chan user _) _) text = postMessage slackToken session (Message chan user text)

defRequest :: CobSession -> Request
defRequest session =
    setRequestManager (tlsmanager session) $
    Net.defaultRequest { secure    = True
                       , port      = 443
                       }

postMessage :: SlackToken -> CobSession -> Message -> IO ()
postMessage slackToken session message = do
    let request = setRequestBodyJSON message $
                  addRequestHeader "Authorization" ("Bearer " <> encodeUtf8 slackToken) $
                    (defRequest session)
                      { method = "POST"
                      , host   = "slack.com"
                      , path   = "/api/chat.postMessage" }
    response <- httpNoBody request
    print response


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

handler :: SlackToken -> CobSession -> Server SlackEvents
handler slackToken session = slackEvent
            
    where
    slackEvent :: SlackEvent -> Handler Text
    slackEvent = \case
        UrlVerification x -> urlVerification x
        WrappedEvent m@(SlackEventWrapper Message{} _) -> message m

    urlVerification :: UrlVerification' -> Handler Text
    urlVerification v = do
        liftIO $ print v
        return $ _challenge v

    message :: SlackEventWrapper Message -> Handler Text
    message m = do
        liftIO $
            runCobT session (commandHandler (getServerId m) (getUserId m) (text $ event m) (createReaction slackToken session m) (replyToMsg slackToken session m))
             >>= either (replyToMsg slackToken session m . pack) return
        return ""


app :: SlackToken -> CobSession -> Application
app slackToken session = serve slackEventsAPI (handler slackToken session)


main :: IO ()
main = do
    slackToken  <- T.init <$> TIO.readFile "slack-token.secret"
    host     <- Prelude.init <$> readFile "cob-host.secret"
    cobtoken <- Prelude.init <$> readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    print "Starting..."
    run 25564 (logStdoutDev (app slackToken session))


data UrlVerification' = UrlVerification'
    { _token     :: Text
    , _challenge :: Text }
    deriving (Show)
instance FromJSON UrlVerification' where
     parseJSON = withObject "url_verification" $ \v -> do
         tok <- v .: "token"
         cha <- v .: "challenge"
         return (UrlVerification' tok cha)
