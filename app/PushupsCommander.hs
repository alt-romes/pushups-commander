{-# LANGUAGE OverloadedStrings #-}
module PushupsCommander where

import Data.Aeson
import Discord.Types
import Data.Bifunctor
import Data.Char
import Text.Read
import Data.Text as T

import RecordM


----- Main -----

type Amount = Int
type ErrorMsg = Text

data Exercise = Pushups | Abs | Squats | Kilometers | Unknown Text deriving (Show)

data Command = AddExercise Amount Exercise
             | RmExercise Amount Exercise
             | QueryDatabase Target Exercise
             deriving (Show)

data Target = Today | All | Server deriving (Show)


parseMsg :: Text -> Either ErrorMsg Command
parseMsg m = case uncons $ whitespace m of
        Just ('+', m') -> do
            (amount, exercise) <- parseAmountAndExercise m'
            return (AddExercise amount exercise)
        Just ('-', m') -> do
            (amount, exercise) <- parseAmountAndExercise m'
            return (RmExercise amount exercise)
        Just ('?', m') -> do
            (target, exercise) <- parseTargetAndExercise m'
            return (QueryDatabase target exercise)
        _     -> Left ""

    where
    parseAmount :: Text -> Either ErrorMsg (Amount, Text)
    parseAmount s = do
        let (a, s') = T.span isDigit $ whitespace s
        amount <- changeLeft ("Couldn't parse amount!" :: Text) $ readEither $ unpack a
        return (amount, s')

    parseExercise :: Text -> Exercise
    parseExercise s = case T.span (/= ' ') $ whitespace s of
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

    parseTarget :: Text -> Either ErrorMsg (Target, Text)
    parseTarget s = case T.span (/= ' ') $ whitespace s of
        ("t", s)      -> return (Today, s)
        ("today", s)  -> return (Today, s)
        ("a", s)      -> return (All, s)
        ("all", s)    -> return (All, s)
        ("s", s)      -> return (Server, s)
        ("server", s) -> return (Server, s)
        _             -> Left "Query should specify today, all, or server"

    whitespace :: Text -> Text
    whitespace = T.dropWhile (== ' ')

    parseAmountAndExercise :: Text -> Either ErrorMsg (Amount, Exercise)
    parseAmountAndExercise s = second parseExercise <$> parseAmount s

    parseTargetAndExercise :: Text -> Either ErrorMsg (Target, Exercise)
    parseTargetAndExercise s = second parseExercise <$> parseTarget s

    changeLeft :: a -> Either b c -> Either a c
    changeLeft newValue eitherValue = case eitherValue of
        Left _ -> Left newValue
        Right r -> Right r



---- Interaction with RecordM -----

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
    definitionName _ = "ROMES Pushups Commander"

makePRecord :: Message -> Amount -> Exercise -> PushupsRecord
makePRecord m = PushupsRecord (messageGuild m) (userId $ messageAuthor m) (userName $ messageAuthor m)

addExercise :: Session -> PushupsRecord -> IO ()
addExercise = integrationPOST

