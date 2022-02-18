{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module DiscordBot where

import Debug.Trace
import Data.Aeson
import Data.Aeson.Types

import Data.Function
import Data.List
import Data.Text as T (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

import Data.Maybe

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader (MonadReader, ask, runReader)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Discord
import Discord.Types as DT
import qualified Discord.Requests as R

import Cob
import PushupsCommander
import Bots

----- Chat Bot -----------

instance ChatBotMessage Message where
    getServerId m = (maybe ("Discord-DM-" <> getUserId m) (pack . show) . messageGuildId) m
    getUserId   = pack . show . DT.userId . messageAuthor
    getContent  = messageContent
    isFromBot   = userIsBot . messageAuthor

instance Chattable DiscordHandler Message where
    reactTo m = void . restCall . R.CreateReaction (messageChannelId m, messageId m)
    replyTo m text = void . restCall $
      R.CreateMessageDetailed (messageChannelId m) $
        def { R.messageDetailedContent = text
            , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

----- Discord ------------

discordHandler :: Bot DiscordHandler Message [ChatBotCommand] -> Event -> DiscordHandler ()
discordHandler bot event = case event of
    MessageCreate m -> runChatBot bot m
    _ -> return ()

discordBot :: BotToken -> Bot Identity (Bot DiscordHandler Message [ChatBotCommand]) RunDiscordOpts
discordBot discordToken = Bot $ \bot -> do
    pure def
         { discordToken = discordToken
         , discordOnStart = liftIO $ TIO.putStrLn "Started"
         , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
         , discordOnEvent = discordHandler bot
         , discordOnLog = TIO.putStrLn
         , discordForkThreadForEvents = True }

discordServer :: BotToken -> ChatBotServer
discordServer = ChatBotServer . discordBot

instance Runnable RunDiscordOpts where
    run _ = TIO.putStrLn <=< runDiscord 
