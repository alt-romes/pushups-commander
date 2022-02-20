{-# LANGUAGE LambdaCase, TemplateHaskell, OverloadedStrings #-}
module PushupsRecordM where

import Control.Monad
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Text

import Data.Aeson

import {-# SOURCE #-} PushupsCommander (Amount, Exercise(..))

import Cob
import Cob.RecordM
import Cob.RecordM.TH

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
type ProfilePicture = Text
type ServerIdentifier = Text
type ServerUsername = Text
type Id = Int

data UsersRecord = UsersRecord
    { _masterUsername :: MasterUsername
    , _profilePicture :: ProfilePicture }
mkRecord ''UsersRecord "ROMES Pushups Users" ["Master Username", "Profile Picture"]
makeLenses ''UsersRecord


data ServersRecord = ServersRecord
    { _serverIdentifier :: Maybe ServerIdentifier
    , _activationCode   :: ActivationCode
    , _owner            :: Owner
    } deriving (Show)
mkRecord ''ServersRecord "ROMES Pushups Servers" ["Server", "Activation Code", "Owner"]
makeLenses ''ServersRecord


data ServerUsersRecord = ServerUsersRecord
    { _userId         :: Ref UsersRecord
    , _serverId       :: Ref ServersRecord
    , _serverUsername :: ServerUsername
    } deriving (Show)
mkRecord ''ServerUsersRecord "ROMES Pushups Server Users" ["Master Username", "Server", "Server Username"]
makeLenses ''ServerUsersRecord


data ExercisesRecord = ExercisesRecord
    { _serverUserId :: Ref ServerUsersRecord
    , _amount       :: Amount
    , _exercise     :: Exercise }
mkRecord ''ExercisesRecord "ROMES Pushups Exercises" ["Server Username", "Amount", "Exercise Type"]
makeLenses ''ExercisesRecord

