{-# LANGUAGE OverloadedStrings #-}
module PushupsRecordM where

import Data.Text

import Data.Aeson

import RecordM
import PushupsCommander


usersDefinition :: Definition UsersRecord
usersDefinition = Definition "ROMES Pushups Users"

serversDefinition :: Definition ServersRecord
serversDefinition = Definition "ROMES Pushups Servers"

serverUsersDefinition :: Definition ServerUsersRecord
serverUsersDefinition = Definition "ROMES Pushups Server Users"

exercisesDefinition :: Definition ExercisesRecord
exercisesDefinition = Definition "ROMES Pushups Exercises"


type MasterUsername = Text
type ServerIdentifier = Text
type ServerUsername = Text
type Id = Int

instance ToJSON Exercise where
    toJSON = toJSON . toLower . pack . show

newtype Ref a = Ref Int
instance ToJSON (Ref a) where
    toJSON (Ref i) = toJSON i

newtype UsersRecord = UsersRecord MasterUsername 
instance ToJSON UsersRecord where
    toJSON (UsersRecord mu) = object
        [ "Master Username" .= mu ]

newtype ServersRecord = ServersRecord ServerIdentifier
instance ToJSON ServersRecord where
    toJSON (ServersRecord si) = object
        [ "Server" .= si ]

data ServerUsersRecord = ServerUsersRecord (Ref UsersRecord) (Ref ServersRecord) ServerUsername
instance ToJSON ServerUsersRecord where
    toJSON (ServerUsersRecord refusers refservers serverusername) = object
        [ "Master Username" .= refusers
        , "Server" .= refservers
        , "Server Username" .= serverusername ]

data ExercisesRecord = ExercisesRecord (Ref ServerUsersRecord) Amount Exercise
instance ToJSON ExercisesRecord where
    toJSON (ExercisesRecord refserverusers amount exercise) = object
        [ "Server Username" .= refserverusers
        , "Amount" .= amount
        , "Exercise Type" .= exercise ]
