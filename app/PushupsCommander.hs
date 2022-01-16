{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module PushupsCommander where

import GHC.Generics
import Data.Functor
import Data.Bifunctor
import Data.Char
import Text.Read
import Data.Text as T
import Control.Monad.Trans.Except

import RecordM


----- Main -----

type Amount = Int

data Exercise = Pushups | Abs | Squats | Kilometers | Unknown Text deriving (Generic, Show)

data Command = AddExercise Amount Exercise
             | RmExercise Amount Exercise
             | QueryDatabase Target Exercise
             | Imperative ImperativeCommand
             | Ok
             deriving (Show)

newtype ImperativeCommand = ActivateCommander Text
                          deriving (Show)

data Target = Today | All | Server deriving (Show, Eq)

-- data ParseError = FailParseAmount | FailNonEmptyWord | FailArgument deriving (Show)
type ParseError = String

parseMsg :: Monad m => Text -> ExceptT ParseError m Command
parseMsg m = case uncons $ whitespace m of
        Just ('+', m') -> parseAmountAndExercise m' <&> uncurry AddExercise
        Just ('-', m') -> parseAmountAndExercise m' <&> uncurry RmExercise
        Just ('?', m') -> parseTargetAndExercise m' <&> maybe Ok (uncurry QueryDatabase)
        Just ('!', m') -> parseImperativeCommand m' <&> maybe Ok Imperative
        _     -> return Ok

    where
    parseImperativeCommand :: Monad m => Text -> ExceptT ParseError m (Maybe ImperativeCommand)
    parseImperativeCommand s = case takeWord s of
        ("activate", s') -> Just . ActivateCommander <$> parseNonEmptyWord s'
        _ -> return Nothing

    parseNonEmptyWord :: Monad m => Text -> ExceptT ParseError m Text
    parseNonEmptyWord s = case takeWord s of
        ("", _) -> throwE "Parser failed expecting a word"
        (w, _) -> return w

    parseAmount :: Monad m => Text -> ExceptT ParseError m (Amount, Text)
    parseAmount s = do
        let (a, s') = T.span isDigit $ whitespace s
        amount <- except $ readEither $ unpack a -- TODO: better error message
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

    parseTarget :: Monad m => Text -> ExceptT ParseError m (Maybe (Target, Text))
    parseTarget s = case takeWord s of
        ("t", s')      -> return $ Just (Today, s')
        ("today", s')  -> return $ Just (Today, s')
        ("a", s')      -> return $ Just (All, s')
        ("all", s')    -> return $ Just (All, s')
        ("s", s')      -> return $ Just (Server, s')
        ("server", s') -> return $ Just (Server, s')
        _              -> return Nothing

    whitespace :: Text -> Text
    whitespace = T.dropWhile (== ' ')

    parseAmountAndExercise :: Monad m => Text -> ExceptT ParseError m (Amount, Exercise)
    parseAmountAndExercise s = second parseExercise <$> parseAmount s

    parseTargetAndExercise :: Monad m => Text -> ExceptT ParseError m (Maybe (Target, Exercise))
    parseTargetAndExercise s = (second parseExercise <$>) <$> parseTarget s

    takeWord :: Text -> (Text, Text)
    takeWord = T.span (/= ' ') . whitespace

