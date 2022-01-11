{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.IO.Class

import Discord
import Discord.Types
import qualified Discord.Requests as R

import RecordM
import PushupsCommander
import PushupsRecordM


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

              QueryDatabase target exercise ->
                  void . restCall $ R.CreateMessage (messageChannel m) "Query database!"

          Left ""  -> return ()

          Left err -> void . restCall $
                  R.CreateMessageDetailed (messageChannel m) $
                    def { R.messageDetailedContent = "Error: " <> err
                        , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

    _ -> return () 

    where
    log :: MonadIO m => Text -> m ()
    log = void . liftIO . TIO.putStrLn

    fromBot :: Message -> Bool
    fromBot = userIsBot . messageAuthor

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

