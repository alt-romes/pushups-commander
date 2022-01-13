{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module PushupsCommander where

import GHC.Generics
import Data.Bifunctor
import Data.Char
import Text.Read
import Data.Text as T

import RecordM


----- Main -----

type Amount = Int

data Exercise = Pushups | Abs | Squats | Kilometers | Unknown Text deriving (Generic, Show)

data Command = AddExercise Amount Exercise
             | RmExercise Amount Exercise
             | QueryDatabase Target Exercise
             | Imperative ImperativeCommand
             deriving (Show)

newtype ImperativeCommand = ActivateCommander Text
                          deriving (Show)

data Target = Today | All | Server deriving (Show, Eq)

data ParseError = Ok | FailParseAmount | FailNonEmptyWord | FailArgument deriving (Show)

parseMsg :: Text -> Either ParseError Command
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
        Just ('!', m') -> Imperative <$> parseImperativeCommand m'
        _     -> Left Ok

    where
    parseImperativeCommand :: Text -> Either ParseError ImperativeCommand
    parseImperativeCommand s = case takeWord s of
        ("activate", s') -> ActivateCommander <$> changeLeft FailArgument (parseNonEmptyWord s')
        _ -> Left Ok

    parseNonEmptyWord :: Text -> Either ParseError Text
    parseNonEmptyWord s = case takeWord s of
        ("", _) -> Left FailNonEmptyWord
        (w, _) -> return w

    parseAmount :: Text -> Either ParseError (Amount, Text)
    parseAmount s = do
        let (a, s') = T.span isDigit $ whitespace s
        amount <- changeLeft FailParseAmount $ readEither $ unpack a
        return (amount, s')

    parseExercise :: Text -> Exercise
    parseExercise s = case takeWord s of
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

    parseTarget :: Text -> Either ParseError (Target, Text)
    parseTarget s = case takeWord s of
        ("t", s')      -> return (Today, s')
        ("today", s')  -> return (Today, s')
        ("a", s')      -> return (All, s')
        ("all", s')    -> return (All, s')
        ("s", s')      -> return (Server, s')
        ("server", s') -> return (Server, s')
        _             -> Left Ok

    whitespace :: Text -> Text
    whitespace = T.dropWhile (== ' ')

    parseAmountAndExercise :: Text -> Either ParseError (Amount, Exercise)
    parseAmountAndExercise s = second parseExercise <$> parseAmount s

    parseTargetAndExercise :: Text -> Either ParseError (Target, Exercise)
    parseTargetAndExercise s = second parseExercise <$> parseTarget s

    takeWord :: Text -> (Text, Text)
    takeWord = T.span (/= ' ') . whitespace

    changeLeft :: a -> Either b c -> Either a c
    changeLeft newValue eitherValue = case eitherValue of
        Left _ -> Left newValue
        Right r -> Right r

