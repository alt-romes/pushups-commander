{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.Text.IO as TIO
import Data.Text as T

import Control.Monad
import Data.Aeson
import Servant
import Servant.API
import Servant.API.ContentTypes
import Network.Wai.Handler.Warp

import PushupsCommander

type SlackEvents =
    ReqBody '[JSON] UrlVerification :> Post '[PlainText] Text

slackEventsAPI :: Proxy SlackEvents
slackEventsAPI = Proxy

handler :: Server SlackEvents
handler = urlVerification
    where

    urlVerification :: UrlVerification -> Handler Text
    urlVerification v = do
        when (_type v /= "url_verification") (throwError err400)
        return $ _challenge v

app :: Application
app = serve slackEventsAPI handler

main :: IO ()
main = do
    slackToken  <- T.init <$> TIO.readFile "slack-token.secret"
    run 25565 app





data UrlVerification = UrlVerification
    { _type      :: Text
    , _token     :: Text
    , _challenge :: Text }
instance FromJSON UrlVerification where
     parseJSON = withObject "url_verification" $ \v -> do
         typ <- v .: "type"
         tok <- v .: "token"
         cha <- v .: "challenge"
         return (UrlVerification typ tok cha)
