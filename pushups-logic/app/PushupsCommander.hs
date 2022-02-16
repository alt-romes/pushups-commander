{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module PushupsCommander where

import Control.Monad.Reader
import Data.Maybe (isNothing, listToMaybe)
import Data.List (find)
import Control.Lens ((^?), (^.), (.~), (?~))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Bifunctor (second)
import Data.Profunctor (dimap)
import Data.Char
import Text.Read hiding (lift)
import Data.Text as T (Text, span, dropWhile, pack, unpack, uncons)
import Data.Text.IO as TIO
import Control.Monad.Trans
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except

import Cob
import Cob.RecordM
import PushupsRecordM
import Bots hiding (Ok)
import qualified Bots (ChatBotCommands(Ok))

----- Main -----

type Amount = Int
data Exercise = Pushups | Abs | Squats | Kilometers | Unknown Text deriving (Show)
data Command = AddExercise Amount Exercise
             | RmExercise Amount Exercise
             -- | QueryDatabase Target Exercise
             | Imperative ImperativeCommand
             | Ok
             deriving (Show)
data ImperativeCommand = ActivateCommander Text
                        | SetMasterUsername Text
                        deriving (Show)
data Target = Today | All | Server deriving (Show, Eq)

pushupsBot :: (ChatBotMessage i, MonadIO m) => ChatBot (CobT m) () i
pushupsBot =
    dimap (\m -> (CobT (parseMsg (getContent m)), getServerId m, getUserId m)) id
        pushupsCommander

pushupsCommander :: MonadIO m => ChatBot (CobT m) () (CobT m Command, ServerIdentifier, MasterUsername)
pushupsCommander = Bot handler where
    handler (parser, serverId, masterUserId) () = do
        command <- parser
        log command
        case command of
            AddExercise amount exercise -> do
                addExercise amount exercise
                return [ ReactWith "muscle" ]

            RmExercise amount exercise  -> do
                addExercise (-amount) exercise
                return [ ReactWith "thumbsup" ]

            Imperative imp -> case imp of 
                ActivateCommander code -> do
                    let hasActivationCode (_, serverRecord) = serverRecord^.activationCode == code && isNothing (serverRecord^.serverIdentifier)
                    serversRecords         <- rmDefinitionSearch ("activation_code:" <> code)
                    (id, activationRecord) <- find hasActivationCode serversRecords /// throwError "Invalid activation code!" 
                    rmUpdateInstances id (serverIdentifier ?~ serverId)
                    return [ ReplyWith "Successfully activated!" ]

                SetMasterUsername newName -> do
                    [serverUser] <- rmDefinitionSearch_ ("server_username:" <> masterUserId <> " server_reference:" <> serverId) 
                    rmUpdateInstances (serverUser^.userId) (masterUsername .~ newName)
                    return [ ReactWith "thumbsup" ]

            Ok -> return [ Bots.Ok ]
            where
                addExercise :: MonadIO m => Amount -> Exercise -> CobT m (Ref ExercisesRecord)
                addExercise amount exercise = do
                    (serverUserId, _) <- rmGetOrAddInstanceM ("serveruser:" <> serverId <> "-" <> masterUserId) createServerUser
                    rmAddInstance (ExercisesRecord serverUserId amount exercise)
                    where
                        createServerUser = do -- The ServerUsers record in this context will only be created by addExercise if needed. If it isn't needed, this code won't run (see rmGetOrAddInstanceM)
                            (serverId, ServersRecord server _ _) <- rmDefinitionSearch ("server:" <> serverId) >>=
                                                                    (/// throwError "Server hasn't been activated yet!") . listToMaybe
                            (newUserId, _) <- rmGetOrAddInstance ("master_username:" <> masterUserId) (UsersRecord masterUserId)
                            return (ServerUsersRecord newUserId serverId masterUserId)

                log = liftIO . TIO.putStrLn . pack . show


type ParseError = String

parseMsg :: Monad m => Text -> ExceptT ParseError m Command
parseMsg m = case uncons $ whitespace m of
        Just ('+', m') -> parseAmountAndExercise m' <&> uncurry AddExercise
        Just ('-', m') -> parseAmountAndExercise m' <&> uncurry RmExercise
        -- Just ('?', m') -> parseTargetAndExercise m' <&> maybe Ok (uncurry QueryDatabase)
        Just ('!', m') -> parseImperativeCommand m' <&> maybe Ok Imperative
        _     -> return Ok

    where
    parseImperativeCommand :: Monad m => Text -> ExceptT ParseError m (Maybe ImperativeCommand)
    parseImperativeCommand s = case takeWord s of
        ("activate", s') -> Just . ActivateCommander <$> parseNonEmptyWord s'
        ("setName", s') -> Just . SetMasterUsername <$> parseNonEmptyWord s'
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

