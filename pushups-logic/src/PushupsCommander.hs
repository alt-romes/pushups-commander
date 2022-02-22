{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module PushupsCommander where

import Prelude hiding ((.))

import System.Random.Stateful (uniformM, globalStdGen)

import Control.Category
import Control.Monad.Reader
import Data.Maybe (isNothing, listToMaybe)
import Data.List (find)
import Control.Lens ((^?), (^.), (.~), (?~))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Bifunctor (first, second)
import Data.Profunctor (dimap)
import Data.Char as Char (toLower, isDigit)
import Text.Read hiding (lift)
import Data.Text as T (Text, span, dropWhile, pack, unpack, uncons, toLower)
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
data Exercise = Pushups | Abs | Squats | Kilometers deriving (Show, Eq)
data ServerPlan = Small | Medium | Large | Huge deriving (Show)
data Command = AddExercise Amount Exercise Text
             | RmExercise Amount Exercise Text
             -- | QueryDatabase Target Exercise
             | Imperative ImperativeCommand
             | Ok
             deriving (Show, Eq)
data ImperativeCommand = ActivateCommander Text
                       | SetMasterUsername Text
                       | SetProfilePicture Text
                       | LinkMasterUsername Text
                       | GetLinkingCode
                       deriving (Show, Eq)
data Target = Today | All | Server deriving (Show, Eq)

pushupsBot :: (ChatBotMessage i, MonadIO m) => Bot (CobT m) i [ChatBotCommand]
pushupsBot = pushupsCommander . processMessageBot

processMessageBot :: (MonadIO m, ChatBotMessage i) => Bot (CobT m) i (Command, ServerIdentifier, ServerUsername)
processMessageBot = Bot $ \m -> do
    command <- either throwError return (runExcept (parseMsg (getContent m)))
    return (command, getServerId m, getUserId m)

pushupsCommander :: MonadIO m => Bot (CobT m) (Command, ServerIdentifier, ServerUsername) [ChatBotCommand]
pushupsCommander = Bot handler where
    handler (command, serverId, serverUserId) = do
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
                    (id, _) <- find hasActivationCode serversRecords ?: throwError "Invalid activation code!" 

                    -- Activate server
                    rmUpdateInstance id (serverIdentifier ?~ serverId)

                    return [ ReplyWith "Successfully activated!" ]

                SetMasterUsername newName -> do

                    -- Updated name can't already be in use
                    existingNames <- rmDefinitionSearch_ @UsersRecord ("master_username:" <> newName)
                    unless (null existingNames) $
                        throwError ("Couldn't set username to " <> show newName <> " because that username is already in use.")

                    -- If a new user is created use the new name (the update might be redundant but it's easier than checking)
                    (_, ServerUsersRecord userId _ _) <- getOrCreateServerUser (UsersRecord newName Nothing Nothing) defServerUser
                    rmUpdateInstance userId (masterUsername .~ newName)

                    return [ ReactWith "thumbsup" ]

                SetProfilePicture url -> do

                    -- If a new user is created use the profile picture url (the update might be redundant but it's easier than checking)
                    (_, s@(ServerUsersRecord userId _ _)) <- getOrCreateServerUser (UsersRecord serverUserId (Just url) Nothing) defServerUser
                    rmUpdateInstance userId (profilePicture ?~ url)

                    return [ ReactWith "thumbsup" ]

                LinkMasterUsername code -> do

                    -- Find user record with linking code
                    (linkUserRef, UsersRecord name _ _) <- rmDefinitionSearch code ?:: throwError "Invalid linking code!"

                    -- Set master user of this server user to linking target (the update might be redundant but it's easier than checking)
                    (serverUserRef, _) <- getOrCreateServerUser defUser (\_ serverRef -> ServerUsersRecord linkUserRef serverRef serverUserId)
                    rmUpdateInstance serverUserRef (userId .~ linkUserRef)

                    -- Clear linking code in user record
                    rmUpdateInstance linkUserRef (linkingCode .~ Nothing)

                    return [ ReactWith "thumbsup", ReplyWith ("Successfully linked to " <> name <> "!") ]

                GetLinkingCode -> do

                    randomCode <- T.pack . show . abs <$> uniformM @Int globalStdGen

                    -- If a new user is created use the linking code (the update might be redudant but it's easier than checking)
                    (_, ServerUsersRecord userId _ _) <- getOrCreateServerUser (UsersRecord serverUserId Nothing (Just randomCode)) defServerUser
                    rmUpdateInstance userId (linkingCode ?~ randomCode)
                    
                    return [ ReactWith "thumbsup", ReplyWith randomCode ]

            Ok -> return [ ]
        where
            addExercise :: MonadIO m => Amount -> Exercise -> Text -> CobT m (Ref ExercisesRecord)
            addExercise amount exercise obs = do
                (serverUserRef, _) <- getOrCreateServerUser defUser defServerUser
                rmAddInstance (ExercisesRecord serverUserRef amount exercise (Just obs))

            -- The ServerUsers record in this context will only be created by
            -- addExercise if needed. If it isn't needed, the creation code won't run
            -- (see rmGetOrAddInstanceM)
            getOrCreateServerUser :: MonadIO m => UsersRecord -> (Ref UsersRecord -> Ref ServersRecord -> ServerUsersRecord) -> CobT m (Ref ServerUsersRecord, ServerUsersRecord)
            getOrCreateServerUser newUser mkServerUser = rmGetOrAddInstanceM ("serveruser:" <> serverId <> "-" <> serverUserId) $ do

                -- Only create server user if server has been activated
                (serverRef, ServersRecord _ _ _ serverPlan) <- rmDefinitionSearch ("server:" <> serverId) ?:: throwError "Server hasn't been activated yet!"

                -- Only create server user if server hasn't exceeded the plan's user limit
                amount <- rmDefinitionCount @ServerUsersRecord ("server:" <> show serverRef)
                when (amount `exceeds` serverPlan) $
                    throwError ("Server has reached its max user capacity (" <> show (capacity serverPlan) <> ") for its current plan (" <> (map Char.toLower . show) serverPlan <> ")")

                -- Create a new server user and a new master user if one doesn't exist yet
                (newUserId, _) <- rmGetOrAddInstance ("master_username:" <> serverUserId) newUser
                return (mkServerUser newUserId serverRef)

            amount `exceeds` serverPlan = amount >= capacity serverPlan
            capacity serverPlan = case serverPlan of
                Small  -> 25
                Medium -> 60
                Large  -> 200
                Huge   -> 1000
            log = liftIO . TIO.putStrLn . pack . show
            defUser = UsersRecord serverUserId Nothing Nothing
            defServerUser userRef serverRef = ServerUsersRecord userRef serverRef serverUserId


type ParseError = String
parseMsg :: Text -> Except ParseError Command
parseMsg m = case uncons $ whitespace m of
        Just ('+', m') -> parseAmountAndExercise m' <&> maybe Ok (uncurry3 AddExercise)
        Just ('-', m') -> parseAmountAndExercise m' <&> maybe Ok (uncurry3 RmExercise)
        Just ('!', m') -> parseImperativeCommand m' <&> maybe Ok Imperative
        _              -> return Ok
        -- Just ('?', m') -> parseTargetAndExercise m' <&> maybe Ok (uncurry QueryDatabase)

    where
    parseImperativeCommand :: Text -> Except ParseError (Maybe ImperativeCommand)
    parseImperativeCommand s = case takeWord s of
        ("activate", s')      -> Just . ActivateCommander  <$> parseNonEmptyWord s'
        ("setName", s')       -> Just . SetMasterUsername  <$> parseNonEmptyWord s'
        ("setProfilePic", s') -> Just . SetProfilePicture  <$> parseNonEmptyWord s'
        ("link", s')          -> Just . LinkMasterUsername <$> parseNonEmptyWord s'
        ("getLinkCode", _)    -> return (Just GetLinkingCode)
        _                     -> return Nothing

    parseNonEmptyWord :: Text -> Except ParseError Text
    parseNonEmptyWord s = case takeWord s of
        ("", _) -> throwError "Parser failed expecting a word"
        (w, _)  -> return w

    parseAmount :: Text -> Except ParseError (Maybe (Amount, Text))
    parseAmount s = do
        let (a, s') = T.span isDigit $ whitespace s
        return ((, s') <$> readMaybe (unpack a))

    parseExercise :: Text -> Except ParseError (Exercise, Text)
    parseExercise s = case first T.toLower (takeWord s) of
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

    parseTarget :: Text -> Except ParseError (Maybe (Target, Text))
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

    parseAmountAndExercise :: Text -> Except ParseError (Maybe (Amount, Exercise, Text))
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

