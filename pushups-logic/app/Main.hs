{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (pack, init, Text)
import Data.ByteString (ByteString(..))
import Data.Text.Encoding (encodeUtf8)
import Servant (Server, Proxy(..), Handler)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless)

import Cob
import Bots
import SlackBot
import DiscordBot
import PushupsCommander

echoBot :: ChatBot
echoBot = Bot handler
    where
    handler m = if isFromBot m then pure [] else
        pure [ReactWith "slight_smile", ReplyWith ("Echo: " <> getContent m)]


main :: IO ()
main = do
    slackToken   <- T.init <$> TIO.readFile "slack-token.secret"
    discordToken <- T.init <$> TIO.readFile "discord-token.secret"
    host     <- init <$> readFile "cob-host.secret"
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobToken
    putStrLn "Starting..."
    runBotServersIO 25564

        ( pushupsBot session )

        [ slackServer (slackToken, session)
        , discordServer discordToken ]
