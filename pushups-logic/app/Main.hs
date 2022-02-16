{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (init, Text)
import Data.ByteString (ByteString(..))
import Data.Text.Encoding (encodeUtf8)
import Servant (Server, Proxy(..), Handler)

import Cob
import ChatBot
import SlackBot
import PushupsCommander

main :: IO ()
main = do
    slackToken  <- T.init <$> TIO.readFile "slack-token.secret"
    host     <- init <$> readFile "cob-host.secret"
    cobtoken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    print "Starting..."
    runBotServers
        25564
        pushupsBot
        [ slackBot ]
        (map (, session) [ slackToken ])
