{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Aeson.Types

import Data.List
import Data.Text as T hiding (any, find)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

import Data.Maybe

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Discord
import Discord.Types
import qualified Discord.Requests as R

import RecordM
import PushupsRecordM
import PushupsCommander


eventHandler :: Session -> Event -> DiscordHandler ()
eventHandler session event = case event of
    MessageCreate m -> unless (fromBot m) $ do
        case parseMsg $ messageText m of
          Right command -> do
            log ("Got command!\n" <> pack (show command))
            case command of

              AddExercise amount exercise -> do
                  liftIO $ addExercise session $ makePRecord m amount exercise
                  void . restCall $ R.CreateReaction (messageChannel m, messageId m) "muscle"

              RmExercise amount exercise -> do
                  liftIO $ addExercise session $ makePRecord m (-amount) exercise
                  void . restCall $ R.CreateReaction (messageChannel m, messageId m) "thumbsup"

              Imperative (ActivateCommander code) -> do
                  res <- liftIO $ runExceptT $ do
                      serversRecords <- rmDefinitionSearch session serversDefinition defaultRMQuery { rmQ = "activation_code:" <> code }
                      case find ((\r -> serversRecordActivationCode r == code && serversRecordServerIdentifier r == "") . snd) serversRecords of
                         Nothing -> throwE "Invalid activation code!"
                         Just (id, activationRecord) -> do
                             lift $ rmUpdateInstance session serversDefinition id activationRecord { serversRecordServerIdentifier = getServerId m }
                  case res of
                      Left e  -> replyToMsg m ("Error: " <> pack e)
                      Right _ -> replyToMsg m "Successfully activated!"

              QueryDatabase target exercise -> do
                  amount <- liftIO $ definitionSearch session "" $ someQuery (messageGuild m) (userId $ messageAuthor m) target exercise
                  void . restCall $ R.CreateMessage (messageChannel m) (pack (show target <> ": " <> show amount <> " " <> unpack (toLower (pack $ show exercise)) <> " done!"))

          Left Ok  -> return ()

          Left FailParseAmount -> replyToMsg m "Error: Couldn't parse amount!"

          Left err -> replyToMsg m $ pack $ show err

    _ -> return () 

    where
    log :: MonadIO m => Text -> m ()
    log = void . liftIO . TIO.putStrLn

    fromBot :: Message -> Bool
    fromBot = userIsBot . messageAuthor

    replyToMsg :: Message -> Text -> DiscordHandler ()
    replyToMsg m text = void . restCall $
      R.CreateMessageDetailed (messageChannel m) $
        def { R.messageDetailedContent = text
            , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

    getServerId :: Message -> ServerIdentifier
    getServerId m = case messageGuild m of
                      Nothing -> "Discord direct message"
                      Just id -> pack $ show id


---- Interaction with RecordM -----


pushupsDefinition :: Definition PushupsRecord
pushupsDefinition = Definition "ROMES Pushups Commander"


type Username = Text

data PushupsRecord = PushupsRecord GuildId UserId Username Amount Exercise
                deriving (Show)

instance FromJSON Exercise where
    parseJSON = withText "Exercise" $ \case
          "pushups" -> return Pushups
          "abs" -> return Abs
          "squats" -> return Squats
          "kilometers" -> return Kilometers
          _ -> fail "Error parsing exercise from JSON"

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
makePRecord m = PushupsRecord (fromJust $  messageGuild m) (userId $ messageAuthor m) (userName $ messageAuthor m)

addExercise :: Session -> PushupsRecord -> IO (Maybe (Ref PushupsRecord))
addExercise = flip rmAddInstance pushupsDefinition


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

