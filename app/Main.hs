{-# LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace
import Data.Aeson
import Data.Aeson.Types

import Data.Function
import Data.List
import Data.Text as T hiding (any, find)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Data.Maybe

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Discord
import Discord.Types as DT
import qualified Discord.Requests as R

import RecordM
import PushupsRecordM
import PushupsCommander

eventHandler :: RMSession -> Event -> DiscordHandler ()
eventHandler session event = case event of
    MessageCreate m -> unless (userIsBot $ messageAuthor m) $
        runCobT session (commandHandler (getServerId m) (getUserId m) (messageContent m) (createReaction m) (replyToMsg m))
         >>= either (replyToMsg m . pack . show) return
    _ -> return ()

    where
        getServerId m = (maybe ("Discord-DM-" <> getUserId m) (pack . show) . messageGuildId) m
        getUserId = pack . show . DT.userId . messageAuthor


replyToMsg :: Message -> Text -> DiscordHandler ()
replyToMsg m text = void . restCall $
  R.CreateMessageDetailed (messageChannelId m) $
    def { R.messageDetailedContent = text
        , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

createReaction :: Message -> Text -> DiscordHandler ()
createReaction m = void . restCall . R.CreateReaction (messageChannelId m, messageId m)

----- Run Discord Bot -----

main :: IO ()
main = do 
    disctok  <- TIO.readFile "discord-token.secret"
    host     <- BS.init <$> BS.readFile "cob-host.secret"
    cobtoken <- BS.init <$> BS.readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    err <- runDiscord $ def
             { discordToken = disctok
             , discordOnStart = liftIO $ TIO.putStrLn "Started"
             , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
             , discordOnEvent = eventHandler session
             , discordOnLog = TIO.putStrLn
             , discordForkThreadForEvents = True }
    TIO.putStrLn err -- log breaking error



-- createMessage :: Message -> Text -> DiscordHandler ()
-- createMessage m = void . restCall . R.CreateMessage (messageChannel m)

-- someQuery :: Maybe GuildId -> UserId -> Target -> Exercise -> Value
-- someQuery serverID userID target exercise = object
--     [ "query" .= object
--         [ "bool" .= object
--             [ "must" .= object
--                 [ "query_string" .= object
--                     [ "query" .= ("user:" <> show userID <>
--                                  " AND server:" <> maybe "*" show serverID <>
--                                  (if target == Today then " AND date.date:now\\/d" else "") <>
--                                  " AND type:" <> unpack (toLower (pack $ show exercise)))
--                     , "analyze_wildcard" .= True ] ]
--             , "must_not" .= emptyArray ] ] 
--     , "aggs"  .= object
--         [ "soma" .= object
--             [ "sum" .= object
--                 [ "field" .= ("amount" :: Text) ] ] ] ]
--
-- QueryDatabase target exercise -> do
--     amount <- liftIO $ definitionSearch s "" $ someQuery (messageGuild m) (DT.userId $ messageAuthor m) target exercise
--     createMessage m (pack (show target <> ": " <> show amount <> " " <> unpack (toLower (pack $ show exercise)) <> " done!")) & lift

