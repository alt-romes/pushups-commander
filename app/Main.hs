{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import Prelude hiding (log)
import Data.Either
import Text.Read
import Data.Char
import Data.Bifunctor
import Control.Monad
import Control.Monad.IO.Class
import Data.Text as T (pack, unpack, isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent
import Discord
import Discord.Types
import qualified Discord.Requests as R


main :: IO ()
main = do 
    tok <- TIO.readFile "auth-token.secret"
    err <- runDiscord $ def
             { discordToken = tok
             , discordOnStart = liftIO $ TIO.putStrLn "Started"
             , discordOnEnd = liftIO $ TIO.putStrLn "Ended"
             , discordOnEvent = eventHandler
             , discordOnLog = TIO.putStrLn
             , discordForkThreadForEvents = True }
    TIO.putStrLn err -- log breaking error

type Amount = Int

type ErrorMsg = Text

data Exercise = Pushups | Abs | Squats | Kilometers | Unknown String
    deriving (Show)

data Command = AddExercise Amount Exercise
             | RmExercise Amount Exercise
             | QueryDatabase Target Exercise
    deriving (Show)

data Target = Today | All | Server
    deriving (Show)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> unless (fromBot m) $ do
        case parseMsg m of
          Right command -> do
            log ("Got command!\n" <> pack (show command))
            case command of

              AddExercise amount exercise -> void $ restCall $
                      R.CreateReaction (messageChannel m, messageId m) "muscle"
              RmExercise amount exercise -> void $ restCall $
                      R.CreateReaction (messageChannel m, messageId m) "thumbsup"
              QueryDatabase target exercise -> void $ restCall $
                      R.CreateMessage (messageChannel m) "Query database!"

          Left ""  -> return ()

          Left err -> void $ restCall $
                  R.CreateMessageDetailed (messageChannel m) $
                    def { R.messageDetailedContent = ":::" <> err
                        , R.messageDetailedReference = Just $ def { referenceMessageId = Just (messageId m) } }

    _ -> return ()

parseMsg :: Message -> Either ErrorMsg Command
parseMsg = parseMsg' . unpack . messageText
    where
    parseMsg' :: String -> Either ErrorMsg Command
    parseMsg' m = case whitespace m of
        '+':m -> do
            (amount, exercise) <- parseAmountAndExercise m
            return (AddExercise amount exercise)
        '-':m -> do
            (amount, exercise) <- parseAmountAndExercise m
            return (RmExercise amount exercise)
        '?':m -> do
            (target, exercise) <- parseTargetAndExercise m
            return (QueryDatabase target exercise)
        _     -> Left ""
    
    parseAmount :: String -> Either ErrorMsg (Amount, String)
    parseAmount s = do
        let (a, s') = span isDigit $ whitespace s
        amount <- changeLeft ("Couldn't parse amount!" :: Text) $ readEither a
        return (amount, s')

    parseExercise :: String -> Exercise
    parseExercise s = case span (/= ' ') $ whitespace s of
        ("", _)           -> Pushups
        ("p", _)          -> Pushups
        ("pushups", _)    -> Pushups
        ("a", _)          -> Abs
        ("abs", _)        -> Abs
        ("s", _)          -> Squats
        ("squats", _)     -> Squats
        ("km", _)         -> Kilometers
        ("kilometers", _) -> Kilometers
        _                 -> Unknown $ whitespace s

    parseTarget :: String -> Either ErrorMsg (Target, String)
    parseTarget s = case span (/= ' ') $ whitespace s of
        ("t", s)      -> return (Today, s)
        ("today", s)  -> return (Today, s)
        ("a", s)      -> return (All, s)
        ("all", s)    -> return (All, s)
        ("s", s)      -> return (Server, s)
        ("server", s) -> return (Server, s)
        _             -> Left "Query should specify today, all, or server"

    whitespace :: String -> String
    whitespace = dropWhile (== ' ')

    parseAmountAndExercise :: String -> Either ErrorMsg (Amount, Exercise)
    parseAmountAndExercise s = second parseExercise <$> parseAmount s

    parseTargetAndExercise :: String -> Either ErrorMsg (Target, Exercise)
    parseTargetAndExercise s = second parseExercise <$> parseTarget s


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

log :: MonadIO m => Text -> m ()
log = void . liftIO . TIO.putStrLn

changeLeft :: a -> Either b c -> Either a c
changeLeft newValue eitherValue = case eitherValue of
    Left _ -> Left newValue
    Right r -> Right r
