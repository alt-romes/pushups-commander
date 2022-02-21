{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Concurrent (threadDelay)

import Cob
import Cob.RecordM
import PushupsRecordM
import Bots

----- Main -----

type Amount = Int
data Exercise = Pushups | Abs | Squats | Kilometers deriving (Show)
data ServerPlan = Small | Medium | Large | Huge deriving (Show)
data Command = AddExercise Amount Exercise Text
             | RmExercise Amount Exercise Text
             -- | QueryDatabase Target Exercise
             | Imperative ImperativeCommand
             | Ok
             deriving (Show)
data ImperativeCommand = ActivateCommander Text
                       | SetMasterUsername Text
                       | SetProfilePicture Text
                       deriving (Show)
data Target = Today | All | Server deriving (Show, Eq)

pushupsCommander :: (ChatBotMessage i, MonadIO m) => Bot (CobT m) i [ChatBotCommand]
pushupsCommander = Bot handler where
    handler m = do
        command <- parseMsg (getContent m)
        log command
        case command of
            AddExercise amount exercise obs -> do
                addExercise amount exercise obs
                return [ ReactWith "muscle" ]

            RmExercise amount exercise obs -> do
                addExercise (-amount) exercise obs
                return [ ReactWith "thumbsup" ]

            Imperative imp -> case imp of 
                ActivateCommander code -> do

                    serversRecords <- rmDefinitionSearch ("activation_code:" <> code)

                    -- Validate code
                    let hasActivationCode (_, serverRecord) = serverRecord^.activationCode == code && isNothing (serverRecord^.serverIdentifier)
                    (id, _)        <- find hasActivationCode serversRecords /// throwError "Invalid activation code!" 

                    -- Activate server
                    rmUpdateInstances id (serverIdentifier ?~ serverId)
                    return [ ReplyWith "Successfully activated!" ]

                SetMasterUsername newName -> do

                    (_, ServerUsersRecord userId _ _) <- getOrCreateServerUser

                    -- Updated name can't already be in use
                    existingNames <- rmDefinitionSearch_ @UsersRecord ("master_username:" <> newName)
                    unless (null existingNames) $
                        throwError ("Couldn't set username to " <> show newName <> " because that username is already in use.")

                    -- Update name
                    rmUpdateInstances userId (masterUsername .~ newName)
                    return [ ReactWith "thumbsup" ]

                SetProfilePicture url -> do

                    (_, s@(ServerUsersRecord userId _ _)) <- getOrCreateServerUser
                    print s & liftIO
                    threadDelay 1000000 & liftIO -- TODO: Bug if update is done too soon (user not found...)

                    -- Update profile picture url
                    rmUpdateInstances userId (profilePicture ?~ url)
                    return [ ReactWith "thumbsup" ]

            Ok -> return [ ]
        where
            addExercise :: MonadIO m => Amount -> Exercise -> Text -> CobT m (Ref ExercisesRecord)
            addExercise amount exercise obs = do
                (serverUserId, _) <- getOrCreateServerUser
                rmAddInstance (ExercisesRecord serverUserId amount exercise (Just obs))

            -- The ServerUsers record in this context will only be created by
            -- addExercise if needed. If it isn't needed, the creation code won't run
            -- (see rmGetOrAddInstanceM)
            getOrCreateServerUser :: MonadIO m => CobT m (Ref ServerUsersRecord, ServerUsersRecord)
            getOrCreateServerUser = rmGetOrAddInstanceM ("serveruser:" <> serverId <> "-" <> serverUserId) $ do

                -- Only create server user if server has been activated
                serversRecords <- rmDefinitionSearch ("server:" <> serverId)
                (serverRef, ServersRecord _ _ _ serverPlan) <- safeHead serversRecords /// throwError "Server hasn't been activated yet!"

                -- Only create server user if server hasn't exceeded the plan's user limit
                amount <- rmDefinitionCount @ServerUsersRecord ("server:" <> show serverRef)
                when (amount `exceeds` serverPlan) $
                    throwError ("Server has reached its max user capacity (" <> show (capacity serverPlan) <> ") for its current plan (" <> (map toLower . show) serverPlan <> ")")

                -- Create a new server user and a new master user if one doesn't exist yet
                (newUserId, _) <- rmGetOrAddInstance ("master_username:" <> serverUserId) (UsersRecord serverUserId Nothing)
                return (ServerUsersRecord newUserId serverRef serverUserId)

            amount `exceeds` serverPlan = amount >= capacity serverPlan
            capacity serverPlan = case serverPlan of
                Small  -> 25
                Medium -> 60
                Large  -> 200
                Huge   -> 1000
            log = liftIO . TIO.putStrLn . pack . show
            serverId = getServerId m
            serverUserId = getUserId m
            safeHead = listToMaybe



parseMsg :: Monad m => Text -> CobT m Command
parseMsg m = case uncons $ whitespace m of
        Just ('+', m') -> parseAmountAndExercise m' <&> maybe Ok (uncurry3 AddExercise)
        Just ('-', m') -> parseAmountAndExercise m' <&> maybe Ok (uncurry3 RmExercise)
        Just ('!', m') -> parseImperativeCommand m' <&> maybe Ok Imperative
        _              -> return Ok
        -- Just ('?', m') -> parseTargetAndExercise m' <&> maybe Ok (uncurry QueryDatabase)

    where
    parseImperativeCommand :: Monad m => Text -> CobT m (Maybe ImperativeCommand)
    parseImperativeCommand s = case takeWord s of
        ("activate", s')      -> Just . ActivateCommander <$> parseNonEmptyWord s'
        ("setName", s')       -> Just . SetMasterUsername <$> parseNonEmptyWord s'
        ("setProfilePic", s') -> Just . SetProfilePicture <$> parseNonEmptyWord s'
        _ -> return Nothing

    parseNonEmptyWord :: Monad m => Text -> CobT m Text
    parseNonEmptyWord s = case takeWord s of
        ("", _) -> throwError "Parser failed expecting a word"
        (w, _) -> return w

    parseAmount :: Monad m => Text -> CobT m (Maybe (Amount, Text))
    parseAmount s = do
        let (a, s') = T.span isDigit $ whitespace s
        return ((, s') <$> readMaybe (unpack a))

    parseExercise :: Monad m => Text -> CobT m (Exercise, Text)
    parseExercise s = case takeWord s of
        (""          , s') -> return (Pushups, s')
        ("p"         , s') -> return (Pushups, s')
        ("pushups"   , s') -> return (Pushups, s')
        ("a"         , s') -> return (Abs, s')
        ("abs"       , s') -> return (Abs, s')
        ("s"         , s') -> return (Squats, s')
        ("squats"    , s') -> return (Squats, s')
        ("km"        , s') -> return (Kilometers, s')
        ("kilometers", s') -> return (Kilometers, s')
        _                  -> throwError "Exercise not recognized"

    parseTarget :: Monad m => Text -> CobT m (Maybe (Target, Text))
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

    parseAmountAndExercise :: Monad m => Text -> CobT m (Maybe (Amount, Exercise, Text))
    parseAmountAndExercise s = do
        mAmount <- parseAmount s
        case mAmount of
          Nothing -> return Nothing
          Just (a, s') -> do
            (e, s'') <- parseExercise s'
            return (Just (a, e, whitespace s'')) -- Return amount, exercise, and observations

    takeWord :: Text -> (Text, Text)
    takeWord = T.span (/= ' ') . whitespace

    uncurry3 f (a, b, c) = f a b c

--     parseTargetAndExercise :: Monad m => Text -> ExceptT ParseError m (Maybe (Target, Exercise))
--     parseTargetAndExercise s = (second parseExercise <$>) <$> parseTarget s

