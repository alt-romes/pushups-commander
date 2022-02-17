{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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

runCobBot :: MonadIO m => CobSession -> ChatBot (CobT m) s i -> ChatBot m s i
runCobBot session = fmap (either (\s -> [ReplyWith $ T.pack s]) id) . transformBot (runCobT session)

echoBot :: (Applicative m, ChatBotMessage i) => ChatBot m () i
echoBot = Bot handler
    where
    handler m () = if isFromBot m then pure [] else
        pure [ReactWith "slight_smile", ReplyWith ("Echo: " <> getContent m)]


main :: IO ()
main = do
    slackToken   <- T.init <$> TIO.readFile "slack-token.secret"
    discordToken <- T.init <$> TIO.readFile "discord-token.secret"
    host     <- init <$> readFile "cob-host.secret"
    cobtoken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    putStrLn "Starting..."
    runBotServers
        25564
        (runCobBot session pushupsBot)
        -- echoBot
        [ slackBot, discordBot ]
        (map (, session) [ slackToken, discordToken ])
