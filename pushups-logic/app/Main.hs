{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (init)
import Data.ByteString (ByteString(..))
import Data.Text.Encoding (encodeUtf8)

import Cob
import ChatBot
import SlackBot

main :: IO ()
main = do
    slackToken  <- T.init <$> TIO.readFile "slack-token.secret"
    host     <- init <$> readFile "cob-host.secret"
    cobtoken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    print "Starting..."
    runChatBots [(ChatBot @SlackEvents (slackHandler slackToken session), 25564)]
