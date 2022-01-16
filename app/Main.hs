{-# LANGUAGE LambdaCase, OverloadedStrings #-}
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

commandHandler :: Message -> CobT DiscordHandler ()
commandHandler m = do
    command <- parseMsg (messageText m)
    log command
    case command of
        AddExercise amount exercise -> do
            addExercise m amount exercise
            createReaction m "muscle" & liftCob

        RmExercise amount exercise  -> do
            addExercise m (-amount) exercise
            createReaction m "thumbsup" & liftCob

        Imperative (ActivateCommander code) -> do
            let hasActivationCode (_, serverRecord) = serverRecord^.activationCode == code
            serversRecords         <- searchServers ("activation_code:" <> code)
            (id, activationRecord) <- find hasActivationCode serversRecords /// throwE "Invalid activation code!" 
            rmUpdateInstance id (activationRecord & serverIdentifier .~ getServerId m)
            replyToMsg m "Successfully activated!" & liftCob

        Ok -> return ()

        -- QueryDatabase target exercise -> do
        --     amount <- liftIO $ definitionSearch s "" $ someQuery (messageGuild m) (DT.userId $ messageAuthor m) target exercise
        --     createMessage m (pack (show target <> ": " <> show amount <> " " <> unpack (toLower (pack $ show exercise)) <> " done!")) & lift

    where
        addExercise :: MonadIO m => Message -> Amount -> Exercise -> CobT m (Ref ExercisesRecord)
        addExercise m amount exercise = do
            (serverUserId, _) <- rmGetOrAddInstanceM ("serveruser:" <> getServerId m <> "-" <> getUserId m) (createServerUser m)
            rmAddInstance (ExercisesRecord serverUserId amount exercise)
            where
                createServerUser m = do -- The ServerUsers record in this context will only be created by addExercise if needed. If it isn't needed, this code won't run (see rmGetOrAddInstanceM)
                    (serverId, ServersRecord server _ _) <- rmDefinitionSearch (defaultRMQuery { _q = "server:" <> getServerId m }) <&> listToMaybe >>=
                                                            (/// throwE "Server hasn't been activated yet!")
                    (newUserId, _) <- rmGetOrAddInstance ("master_username:" <> getUserId m) (UsersRecord $ getUserId m)
                    return (ServerUsersRecord newUserId serverId (getUserId m))

        searchServers rmQ = rmDefinitionSearch (defaultRMQuery { _q = rmQ })
        getServerId = maybe "Discord direct message" (pack . show) . messageGuild
        getUserId = pack . show . DT.userId . messageAuthor
        log = liftIO . TIO.putStrLn . pack . show



----- Run Discord Bot -----

main :: IO ()
main = do 
    tok <- TIO.readFile "discord-token.secret"
    session <- join $ makeSession <$> (BS.init <$> BS.readFile "cob-host.secret") <*> (BS.init <$> BS.readFile "cob-token.secret")
    err <- runDiscord $ def
             { discordToken = tok
             , discordOnStart = liftIO $ TIO.putStrLn "Started"
             , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
             , discordOnEvent = \case
                 MessageCreate m ->
                   runCob session (commandHandler m)
                     >>= either (replyToMsg m . pack . show) return
                 _ -> return ()
             , discordOnLog = TIO.putStrLn
             , discordForkThreadForEvents = True }
    TIO.putStrLn err -- log breaking error


--- Util
replyToMsg :: Message -> Text -> DiscordHandler ()
replyToMsg m text = void . restCall $
  R.CreateMessageDetailed (messageChannel m) $
    def { R.messageDetailedContent = text
        , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

createReaction :: Message -> Text -> DiscordHandler ()
createReaction m = void . restCall . R.CreateReaction (messageChannel m, messageId m)

createMessage :: Message -> Text -> DiscordHandler ()
createMessage m = void . restCall . R.CreateMessage (messageChannel m)


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
