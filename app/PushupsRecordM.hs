{-# LANGUAGE LambdaCase, TemplateHaskell, OverloadedStrings #-}
module PushupsRecordM where

import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Text

import Data.Aeson

import RecordM
import {-# SOURCE #-} PushupsCommander (Amount, Exercise(..))


instance ToJSON Exercise where
    toJSON = toJSON . toLower . pack . show
instance FromJSON Exercise where
    parseJSON = withText "Exercise" $ \case
          "pushups" -> return Pushups
          "abs" -> return Abs
          "squats" -> return Squats
          "kilometers" -> return Kilometers
          _ -> fail "Error parsing exercise from JSON"


type Owner = Text
type ActivationCode = Text
type MasterUsername = Text
type ServerIdentifier = Text
type ServerUsername = Text
type Id = Int

newtype UsersRecord = UsersRecord { _masterUsername :: MasterUsername }
instance Record UsersRecord where
    definition = "ROMES Pushups Users"
instance ToJSON UsersRecord where
    toJSON (UsersRecord mu) = object
        [ "Master Username" .= mu ]
instance FromJSON UsersRecord where
    parseJSON = withObject "user record" $ \v -> do
        [name] <- v .:? "master_username" .!= []
        return (UsersRecord name)

-- Optional fields should be Maybes, unless they have a default value

data ServersRecord = ServersRecord { _serverIdentifier :: ServerIdentifier
                                   , _activationCode   :: ActivationCode
                                   , _owner            :: Owner }
                                   deriving (Show)
instance Record ServersRecord where
    definition = "ROMES Pushups Servers"
instance ToJSON ServersRecord where
    toJSON (ServersRecord si co o) = object
        [ "Server"          .= si
        , "Activation Code" .= co
        , "Owner"           .= o ]
instance FromJSON ServersRecord where
    parseJSON = withObject "server record" $ \v -> do
        -- Important to take into consideration that all values come as arrays,
        -- so we must select the information on parse
        [si] <- v .:? "server"          .!= [""] -- Default value
        [co] <- v .:  "activation_code"          -- Assumed to exist (is mandatory)
        [o]  <- v .:  "owner"
        return (ServersRecord si co o)


data ServerUsersRecord = ServerUsersRecord { _userId         :: Ref UsersRecord
                                           , _serverId       :: Ref ServersRecord
                                           , _serverUsername :: ServerUsername }
                                           deriving (Show)
instance Record ServerUsersRecord where
    definition = "ROMES Pushups Server Users"
instance ToJSON ServerUsersRecord where
    toJSON (ServerUsersRecord (Ref users) (Ref servers) serverUsername) = object
        [ "Master Username" .= show users
        , "Server"          .= show servers
        , "Server Username" .= serverUsername ]
instance FromJSON ServerUsersRecord where
    parseJSON = withObject "server users record" $ \v -> do
        [mu] <- v .: "master_username"
        [s]  <- v .: "server"
        [su] <- v .: "server_username"
        return (ServerUsersRecord (Ref $ read mu) (Ref $ read s) su)

data ExercisesRecord = ExercisesRecord { _serverUserId :: Ref ServerUsersRecord
                                       , _amount       :: Amount
                                       , _exercise     :: Exercise }
instance Record ExercisesRecord where
    definition = "ROMES Pushups Exercises"
instance ToJSON ExercisesRecord where
    toJSON (ExercisesRecord (Ref serverUsers) amount exercise) = object
        [ "Server Username" .= show serverUsers
        , "Amount"          .= show amount
        , "Exercise Type"   .= exercise ]
instance FromJSON ExercisesRecord where
    parseJSON = withObject "exercises record" $ \v -> do
        [su] <- v .: "server_username"
        [a]  <- v .: "amount"
        [e]  <- v .: "exercise_type"
        return (ExercisesRecord (Ref $ read su) (read a) e)

makeLenses ''UsersRecord
makeLenses ''ServersRecord
makeLenses ''ServerUsersRecord
makeLenses ''ExercisesRecord
