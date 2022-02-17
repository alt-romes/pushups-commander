{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Discord
import Discord.Types as DT
import qualified Discord.Requests as R

import Cob
import PushupsCommander
import Bots

-------- Chat Bot -----------

instance ChatBotMessage Message where
    getServerId m = (maybe ("Discord-DM-" <> getUserId m) (pack . show) . messageGuildId) m
    getUserId   = pack . show . DT.userId . messageAuthor
    getContent  = messageContent
    isFromBot   = userIsBot . messageAuthor

instance Chattable DiscordHandler () Message where
    reactTo reader m = void . restCall . R.CreateReaction (messageChannelId m, messageId m)
    replyTo reader m text = void . restCall $
      R.CreateMessageDetailed (messageChannelId m) $
        def { R.messageDetailedContent = text
            , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

----- Run Discord Bot -----

discordHandler :: ChatBot DiscordHandler () Message -> () -> Event -> DiscordHandler ()
discordHandler bot r event = case event of
    MessageCreate m -> runChatBot bot r () m
    _ -> return ()


discordBot :: Applicative m => Bot m Text (ChatBot DiscordHandler () Message) RunDiscordOpts
discordBot = Bot $ \bot disctok -> pure def
                 { discordToken = disctok
                 , discordOnStart = liftIO $ TIO.putStrLn "Started"
                 , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
                 , discordOnEvent = discordHandler bot ()
                 , discordOnLog = TIO.putStrLn
                 , discordForkThreadForEvents = True }

instance Runnable RunDiscordOpts where
    run _ = TIO.putStrLn <=< runDiscord 
