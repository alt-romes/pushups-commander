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

commandHandler :: Session -> Message -> Command -> DiscordHandler()
commandHandler = undefined

eventHandler :: Session -> Event -> DiscordHandler ()
eventHandler session event = case event of
    MessageCreate m -> do
        x <- runExceptT $ do
            command <- parseMsg $ messageText m
            log ("Got command!\n" <> pack (show command))
            case command of

              AddExercise amount exercise -> do
                  addExercise m amount exercise
                  createReaction m "muscle" & lift

              RmExercise amount exercise -> do
                  addExercise m (-amount) exercise
                  createReaction m "thumbsup" & lift

              Imperative (ActivateCommander code) -> do
                  let hasActivationCode (_, serverRecord) = serverRecord^.activationCode == code
                  serversRecords         <- searchServers (defaultRMQuery & q .~ "activation_code:" <> code)
                  (id, activationRecord) <- find hasActivationCode serversRecords /// throwE "Invalid activation code!" 
                  updateServer id (activationRecord & serverIdentifier .~ getServerId m)
                  replyToMsg m "Successfully activated!" & lift

              QueryDatabase target exercise -> do

                  amount <- liftIO $ definitionSearch session "" $ someQuery (messageGuild m) (DT.userId $ messageAuthor m) target exercise
                  createMessage m (pack (show target <> ": " <> show amount <> " " <> unpack (toLower (pack $ show exercise)) <> " done!")) & lift

              Ok -> return ()

        case x of
            Left err -> replyToMsg m $ pack $ show err
            _ -> return ()
    _ -> return () 

    where
    addExercise :: MonadIO m => Message -> Amount -> Exercise -> ExceptT RMError m (Ref ExercisesRecord)
    addExercise m amount exercise = do
        (serverUserId, _) <- rmGetOrAddInstanceM session serverUsersDefinition ("serveruser:" <> getServerId m <> "-" <> getUserId m) (do
                                  (serverId, ServersRecord server _ _) <- rmDefinitionSearch session serversDefinition (defaultRMQuery & q .~ "server:" <> getServerId m) <&> listToMaybe >>= (/// throwE "Server hasn't been activated yet!")
                                  (newUserId, UsersRecord masterUsername) <- rmGetOrAddInstance session usersDefinition ("master_username:" <> getUserId m) (UsersRecord $ getUserId m)
                                  return (ServerUsersRecord newUserId serverId (getUserId m))
                                )
        rmAddInstance session exercisesDefinition (ExercisesRecord serverUserId amount exercise)

    searchServers = rmDefinitionSearch session serversDefinition
    updateServer  = rmUpdateInstance session serversDefinition

    log :: MonadIO m => Text -> m ()
    log = void . liftIO . TIO.putStrLn

    fromBot :: Message -> Bool
    fromBot = userIsBot . messageAuthor

    replyToMsg :: Message -> Text -> DiscordHandler ()
    replyToMsg m text = void . restCall $
      R.CreateMessageDetailed (messageChannel m) $
        def { R.messageDetailedContent = text
            , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

    createReaction :: Message -> Text -> DiscordHandler ()
    createReaction m = void . restCall . R.CreateReaction (messageChannel m, messageId m)

    createMessage :: Message -> Text -> DiscordHandler ()
    createMessage m = void . restCall . R.CreateMessage (messageChannel m)

    getServerId :: Message -> ServerIdentifier
    getServerId m = case messageGuild m of
                      Nothing -> "Discord direct message"
                      Just id -> pack $ show id

    getUserId :: Message -> ServerUsername
    getUserId = pack . show .DT.userId . messageAuthor


---- Interaction with RecordM -----


pushupsDefinition :: Definition PushupsRecord
pushupsDefinition = Definition "ROMES Pushups Commander"


type Username = Text

data PushupsRecord = PushupsRecord GuildId UserId Username Amount Exercise
                deriving (Show)

instance ToJSON PushupsRecord where
    toJSON (PushupsRecord sid uid username amount exercise) = object
        [ "Server" .= sid
        , "User"   .= uid
        , "Name"   .= username
        , "Amount" .= show amount
        , "Type"   .= T.toLower (pack $ show exercise) ]

instance FromJSON PushupsRecord where
    parseJSON = withObject "PushupsRecord" $ \v -> do
        -- Important to take into consideration that all values come as arrays,
        -- so we must select the information on parse
        [server] <- v .: "server"
        [user]   <- v .: "user"
        [name]   <- v .: "name"
        [amount] <- v .: "amount"
        [t]      <- v .: "type"
        return (PushupsRecord server user name (read amount) t)

instance Record PushupsRecord where

makePRecord :: Message -> Amount -> Exercise -> PushupsRecord
makePRecord m = PushupsRecord (fromJust $  messageGuild m) (DT.userId $ messageAuthor m) (userName $ messageAuthor m)


----- Run Discord Bot -----

main :: IO ()
main = do 
    tok <- TIO.readFile "discord-token.secret"
    session <- join $ makeSession <$> (BS.init <$> BS.readFile "cob-host.secret") <*> (BS.init <$> BS.readFile "cob-token.secret")
    err <- runDiscord $ def
             { discordToken = tok
             , discordOnStart = liftIO $ TIO.putStrLn "Started"
             , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
             , discordOnEvent = eventHandler session
             , discordOnLog = TIO.putStrLn
             , discordForkThreadForEvents = True }
    TIO.putStrLn err -- log breaking error


someQuery :: Maybe GuildId -> UserId -> Target -> Exercise -> Value
someQuery serverID userID target exercise = object
    [ "query" .= object
        [ "bool" .= object
            [ "must" .= object
                [ "query_string" .= object
                    [ "query" .= ("user:" <> show userID <>
                                 " AND server:" <> maybe "*" show serverID <>
                                 (if target == Today then " AND date.date:now\\/d" else "") <>
                                 " AND type:" <> unpack (toLower (pack $ show exercise)))
                    , "analyze_wildcard" .= True ] ]
            , "must_not" .= emptyArray ] ] 
    , "aggs"  .= object
        [ "soma" .= object
            [ "sum" .= object
                [ "field" .= ("amount" :: Text) ] ] ] ]


