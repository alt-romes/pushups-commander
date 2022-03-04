{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE BlockArguments #-}
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
import Control.Monad.Identity (Identity)
import Control.Monad.Reader
import Control.Applicative ((<|>), empty)
import Data.Maybe (isNothing, listToMaybe, fromMaybe)
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
import Control.Monad.Except (throwError, MonadError, catchError)
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
                       | CreateChallenge Amount Exercise TimeInterval Text
                       deriving (Show, Eq)
data TimeInterval = Daily
                  | Weekly
                  | Monthly
                  | Yearly
                  deriving (Show, Eq)
data Target = Today | All | Server deriving (Show, Eq)

pushupsBot :: (MonadIO m, ChatBotMessage i) => Bot (RecordM m) i [ChatBotCommand]
pushupsBot = Bot $ \m -> do
    command <- either throwError return $ fromMaybe (Right Ok) $ runExceptT $ runParser parseMsg (getContent m)
    flip runReaderT (getServerId m, getUserId m) $ runBot pushupsCommander command

type PushupsBotM m = ReaderT (ServerIdentifier, ServerUsername) (RecordM m)

pushupsCommander :: MonadIO m => Bot (PushupsBotM m) Command [ChatBotCommand]
pushupsCommander = Bot $ \command -> do
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
                    activateCommander code
                    return [ ReplyWith "Successfully activated!" ]

                SetMasterUsername newName -> do
                    setMasterUsername newName
                    return [ ReactWith "thumbsup" ]

                SetProfilePicture url -> do
                    setProfilePic url
                    return [ ReactWith "thumbsup" ]

                LinkMasterUsername code -> do
                    name <- linkMasterUsername code
                    return [ ReactWith "thumbsup", ReplyWith ("Successfully linked to " <> name <> "!") ]

                GetLinkingCode -> do
                    randomCode <- getLinkingCode
                    return [ ReactWith "thumbsup", ReplyWith randomCode ]

                CreateChallenge amount exercise interval obs -> do
                    createChallenge amount exercise interval (Just obs)
                    return [ ReplyWith "A challenge has been declared:" ]

            Ok -> return [ ]

        where log = liftIO . TIO.putStrLn . pack . show


addExercise :: MonadIO m => Amount -> Exercise -> Text -> PushupsBotM m (Ref ExercisesRecord)
addExercise amount exercise obs = do
    (serverUserRef, _) <- getOrCreateServerUser
    rmAddInstance (ExercisesRecord serverUserRef amount exercise (Just obs)) & lift


activateCommander :: MonadIO m => Text -> PushupsBotM m ServersRecord
activateCommander code = asks fst >>= \serverId -> lift do
    
    serversRecords <- rmDefinitionSearch ("activation_code:" <> code)

    -- Validate code
    let hasActivationCode (_, serverRecord) = serverRecord^.activationCode == code && isNothing (serverRecord^.serverIdentifier)
    (id, _) <- find hasActivationCode serversRecords ?? throwError "Invalid activation code!" 

    -- Activate server
    rmUpdateInstance id (serverIdentifier ?~ serverId)


setMasterUsername :: MonadIO m => Text -> PushupsBotM m UsersRecord
setMasterUsername newName = do
    -- Updated name can't already be in use
    existingNames <- rmDefinitionSearch_ @UsersRecord ("master_username:" <> newName) & lift
    unless (null existingNames) $
        throwError ("Couldn't set username to " <> show newName <> " because that username is already in use.")

    (_, ServerUsersRecord userId _ _) <- getOrCreateServerUser
    rmUpdateInstance userId (masterUsername .~ newName) & lift


setProfilePic :: MonadIO m => Text -> PushupsBotM m UsersRecord
setProfilePic url = do
    (_, s@(ServerUsersRecord userId _ _)) <- getOrCreateServerUser
    rmUpdateInstance userId (profilePicture ?~ url) & lift


linkMasterUsername :: MonadIO m => Text -> PushupsBotM m Text
linkMasterUsername code = do
    -- Find user record with linking code
    (linkUserRef, UsersRecord name _ _) <- rmDefinitionSearch code ??? throwError "Invalid linking code!" & lift

    -- Set this server user's master user to user with given linking code
    (serverUserRef, _) <- getOrCreateServerUser
    rmUpdateInstance serverUserRef (userId .~ linkUserRef) &lift

    -- Clear linking code in user record
    rmUpdateInstance linkUserRef (linkingCode ?~ "") &lift

    return name


getLinkingCode :: MonadIO m => PushupsBotM m Text
getLinkingCode = do
    (_, ServerUsersRecord userId _ _) <- getOrCreateServerUser

    -- Set linking code in this server user's master user
    randomCode <- T.pack . show . abs <$> uniformM @Int globalStdGen
    rmUpdateInstance userId (linkingCode ?~ randomCode) & lift

    return randomCode


createChallenge :: MonadIO m => Amount -> Exercise -> TimeInterval -> Maybe Text -> PushupsBotM m (Ref ChallengesRecord)
createChallenge amount exercise timeInterval obs = asks fst >>= \serverId -> lift do
    (serverRef, ServersRecord {}) <- rmDefinitionSearch ("server:" <> serverId) ??? throwError "Server hasn't been activated yet!"
    rmAddInstance (ChallengesRecord serverRef amount exercise timeInterval obs)



-- The ServerUsers record in this context will only be created by
-- addExercise if needed. If it isn't needed, the creation code won't run
-- (see rmGetOrAddInstanceM)
getOrCreateServerUser :: MonadIO m => PushupsBotM m (Ref ServerUsersRecord, ServerUsersRecord)
getOrCreateServerUser = ask >>= \(serverId :: ServerIdentifier, serverUserId) -> lift do
    (rmDefinitionSearch ("server_reference:" <> serverId <> " server_username:" <> serverUserId) ?!) <|> do
        -- Only create server user if server has been activated
        (serverRef, ServersRecord _ _ _ serverPlan) <- rmDefinitionSearch ("server:" <> serverId) ??? throwError "Server hasn't been activated yet!"

        -- Only create server user if server hasn't exceeded the plan's user limit
        amount <- rmDefinitionCount @ServerUsersRecord ("server:" <> show serverRef)
        when (amount `exceeds` serverPlan) $
            throwError ("Server has reached its max user capacity (" <> show (capacity serverPlan) <> ") for its current plan (" <> (map Char.toLower . show) serverPlan <> ")")

        -- Create a new server user and a new master user (one doesn't exist yet)
        newUserRef <- fst <$> (rmDefinitionSearch ("master_username:" <> serverUserId) ?!) <|> rmAddInstanceSync (UsersRecord serverUserId Nothing Nothing)
        ref <- rmAddInstanceSync (ServerUsersRecord newUserRef serverRef serverUserId)
        return (ref, ServerUsersRecord newUserRef serverRef serverUserId)
    where
        amount `exceeds` serverPlan = amount >= capacity serverPlan
        capacity serverPlan = case serverPlan of
            Small  -> 25
            Medium -> 60
            Large  -> 200
            Huge   -> 1000


newtype ParserT m a = Parser { unParser :: Text -> m (a, Text) }
type CobParser a = ParserT Identity a

instance Functor m => Functor (ParserT m) where
    fmap f (Parser g) = Parser (fmap (first f) . g)

instance Monad m => Applicative (ParserT m) where
    pure x = Parser $ \s -> pure (x, s)
    Parser g <*> Parser h = Parser $ \s -> do
        (t, s') <- g s
        (y, s'') <- h s'
        pure (t y, s'')

instance Monad m => Monad (ParserT m) where
    Parser x' >>= f = Parser $ \s -> do
        (x, s') <- x' s
        unParser (f x) s'

instance Monad m => MonadError String (ParserT (ExceptT String m)) where
    throwError = Parser . const . throwError
    Parser f `catchError` g = Parser $ \s -> f s `catchError` (($ s) . unParser . g)

runParser :: Functor m => ParserT m a -> Text -> m a
runParser p = fmap fst . unParser p

type PushupsParser a = ParserT (ExceptT String Maybe) a

parseMsg :: PushupsParser Command
parseMsg = takeHead >>= \case
        '+' -> AddExercise <$> parseAmount <*> parseExercise <*> takeRemaining
        '-' -> RmExercise  <$> parseAmount <*> parseExercise <*> takeRemaining
        '!' -> Imperative  <$> parseImperativeCommand
        _   -> failPParser

    where
    parseImperativeCommand :: PushupsParser ImperativeCommand
    parseImperativeCommand = takeWord >>= \case
        "activate"      -> ActivateCommander  <$> parseNonEmptyWord
        "setName"       -> SetMasterUsername  <$> parseNonEmptyWord
        "setProfilePic" -> SetProfilePicture  <$> parseNonEmptyWord
        "link"          -> LinkMasterUsername <$> parseNonEmptyWord
        "getLinkCode"   -> return GetLinkingCode
        "challenge"     -> CreateChallenge    <$> parseAmount <*> parseExercise <*> parseInterval <*> takeRemaining
        _               -> failPParser

    parseAmount :: PushupsParser Amount
    parseAmount = Parser $ \s ->
        let (a, s') = T.span isDigit $ whitespace s in
        lift $ (, s') <$> readMaybe (unpack a)

    parseExercise :: PushupsParser Exercise
    parseExercise = takeWord >>= (\case
        ""           -> return Pushups
        "p"          -> return Pushups
        "pushups"    -> return Pushups
        "a"          -> return Abs
        "abs"        -> return Abs
        "s"          -> return Squats
        "squats"     -> return Squats
        "km"         -> return Kilometers
        "kilometers" -> return Kilometers
        _            -> throwError "Exercise not recognized"
                                 ) . T.toLower

    parseInterval :: PushupsParser TimeInterval
    parseInterval = takeWord >>= (\case
        "daily"   -> return Daily
        "weekly"  -> return Weekly
        "monthly" -> return Monthly
        "yearly"  -> return Yearly
        _         -> failPParser
                                 ) . T.toLower

    failPParser :: PushupsParser a
    failPParser = Parser (const $ lift Nothing)


takeWord :: PushupsParser Text
takeWord = Parser (return . T.span (/= ' ') . whitespace)

takeHead :: PushupsParser Char
takeHead = Parser (lift . T.uncons . whitespace)

takeRemaining :: PushupsParser Text
takeRemaining = Parser (return . (, mempty) . whitespace)

parseNonEmptyWord :: PushupsParser Text
parseNonEmptyWord = takeWord >>= \case
    "" -> throwError "Parser failed expecting a word"
    w  -> return w

whitespace :: Text -> Text
whitespace = T.dropWhile (== ' ')

