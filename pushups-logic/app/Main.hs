{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (pack, unpack, init, Text)
import Data.ByteString (ByteString(..))
import Data.Text.Encoding (encodeUtf8)
import Servant (Server, Proxy(..), Handler)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless, (>=>))

import Cob
import Cob.RecordM
import Cob.RecordM.TH
import Bots
import SlackBot
import DiscordBot
import PushupsCommander

---- Classificações

data Classficação = Classficação { thisMonth :: String
                                 , fullName  :: String }
mkRecord ''Classficação "CASA Finanças Classificação" ["This Month", "Full Name"]

instance Show Classficação where
    show (Classficação m f) = f <> ": " <> m <> "\n"

-- cobClassf = Bot (rmDefinitionSearch_ . getContent >=> pure . (:[]) . ReplyWith . T.pack . show @[Classficação])
cobClassf :: (MonadIO m, ChatBotMessage i) => CobBot m i [ChatBotCommand]
cobClassf = Bot $ \m -> do
    classf :: [Classficação] <- rmDefinitionSearch_ (getContent m)
    pure [ReplyWith (T.pack (show classf))]

----

type CobBot m i o = Bot (CobT m) i o

runCobBot :: Monad m => CobSession -> CobBot m i [ChatBotCommand] -> Bot m i [ChatBotCommand]
runCobBot session = transformBot (fmap (either ((:[]) . ReplyWith . T.pack) id) . runCobT session)

echoBot :: ChatBot
echoBot = ChatBot $ Bot handler
    where
    handler m =
        if isFromBot m
           then return []
           else return [ReactWith "slight_smile", ReplyWith ("Echo: " <> getContent m)]

adderBot :: forall m i. (Monad m, ChatBotMessage i) => Bot m i [ChatBotCommand]
adderBot = Bot $ \m ->
    let str2Int = read @Int . T.unpack
        int2Str = T.pack . show @Int in
    if isFromBot m then pure [] else pure [ReplyWith ("Echo: " <> (int2Str . (+1) . str2Int . getContent) m)]

main :: IO ()
main = do
    slackToken   <- T.init <$> TIO.readFile "slack-token.secret"
    discordToken <- T.init <$> TIO.readFile "discord-token.secret"
    host     <- init <$> readFile "cob-host.secret"
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobToken
    putStrLn "Starting..."

    runChatBotServers 25564 echoBot [ slackServer (slackToken, session), discordServer discordToken ]

