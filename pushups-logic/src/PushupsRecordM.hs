{-# LANGUAGE LambdaCase, TemplateHaskell, OverloadedStrings #-}
module PushupsRecordM where

import Control.Monad
import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Text

import Data.Aeson

import {-# SOURCE #-} PushupsCommander (Amount, Exercise(..), ServerPlan(..), TimeInterval(..))

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

instance ToJSON ServerPlan where
    toJSON = toJSON . toLower . pack . show
instance FromJSON ServerPlan where
    parseJSON = withText "Server Plan" $ \case
          "small" -> return Small
          "medium" -> return Medium
          "large" -> return Large
          "huge" -> return Huge
          _ -> fail "Error parsing server plan from JSON"

instance ToJSON TimeInterval where
    toJSON = toJSON . toLower . pack . show
instance FromJSON TimeInterval where
    parseJSON = withText "Time Interval" $ \case
          "day" -> return Day
          "week" -> return Week
          "month" -> return Month
          "year" -> return Year
          _ -> fail "Error parsing time interval from JSON"


type Owner = Text
type ActivationCode = Text
type MasterUsername = Text
type ServerIdentifier = Text
type ServerUsername = Text
type Id = Int


data UsersRecord = UsersRecord
    { _masterUsername :: MasterUsername
    , _profilePicture :: Maybe Text
    , _linkingCode    :: Maybe Text}
mkRecord ''UsersRecord "ROMES Pushups Users" ["Master Username", "Profile Picture", "Linking Code"]
makeLenses ''UsersRecord


data ServersRecord = ServersRecord
    { _serverIdentifier :: Maybe ServerIdentifier
    , _activationCode   :: ActivationCode
    , _owner            :: Owner
    , _serverPlan       :: ServerPlan
    } deriving (Show)
mkRecord ''ServersRecord "ROMES Pushups Servers" ["Server", "Activation Code", "Owner", "Server Plan"]
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
    , _exercise     :: Exercise
    , _obs          :: Maybe Text }
mkRecord ''ExercisesRecord "ROMES Pushups Exercises" ["Server Username", "Amount", "Exercise Type", "Obs"]
makeLenses ''ExercisesRecord


data ChallengesRecord = ChallengesRecord
    { _cServerId :: Ref ServersRecord
    , _cAmount   :: Amount
    , _cExercise :: Exercise
    , _cInterval :: TimeInterval
    , _cObs      :: Maybe Text
    }
mkRecord ''ChallengesRecord "ROMES Pushups Challenges" ["Server", "Amount", "Exercise", "Time Interval", "Obs"]
makeLenses ''ChallengesRecord
