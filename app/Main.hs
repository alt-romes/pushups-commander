{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Aeson.Types

import Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

import Data.Maybe

import Control.Monad
import Control.Monad.IO.Class

import Discord
import Discord.Types
import qualified Discord.Requests as R

import RecordM
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
                  replyToMsg m "Successfully activated!"

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


---- Interaction with RecordM -----


pushupsDefinition :: Definition PushupsRecord
pushupsDefinition = Definition "ROMES Pushups Commander"


type Username = Text

data PushupsRecord = PushupsRecord (Maybe GuildId) UserId Username Amount Exercise

instance ToJSON PushupsRecord where
    toJSON (PushupsRecord sid uid username amount exercise) = object
        [ "Server" .= sid
        , "User"   .= uid
        , "Name"   .= username
        , "Amount" .= show amount
        , "Type"   .= T.toLower (pack $ show exercise) ]

instance Record PushupsRecord where

makePRecord :: Message -> Amount -> Exercise -> PushupsRecord
makePRecord m = PushupsRecord (messageGuild m) (userId $ messageAuthor m) (userName $ messageAuthor m)

addExercise :: Session -> PushupsRecord -> IO ()
addExercise = integrationPOST pushupsDefinition


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

