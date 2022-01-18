-- {-# LANGUAGE OverloadedStrings #-}
module Main where

import RecordM
import PushupsCommander

import qualified Web.Slack as Slack
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    slackToken  <- TIO.readFile "slack-token.secret"
    slackConfig <- Slack.mkSlackConfig slackToken
