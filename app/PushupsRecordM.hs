{-# LANGUAGE OverloadedStrings #-}
module PushupsRecordM where

import Data.Maybe
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


type Owner = Text
type ActivationCode = Text
type MasterUsername = Text
type ServerIdentifier = Text
type ServerUsername = Text
type Id = Int

instance ToJSON Exercise where
    toJSON = toJSON . toLower . pack . show

newtype UsersRecord = UsersRecord MasterUsername 
instance Record UsersRecord
instance ToJSON UsersRecord where
    toJSON (UsersRecord mu) = object
        [ "Master Username" .= mu ]
instance FromJSON UsersRecord where
    parseJSON (Object v) = do
        [name] <- v .: "master_username"
        return (UsersRecord name)

data ServersRecord = ServersRecord { serversRecordServerIdentifier :: ServerIdentifier
                                   , serversRecordActivationCode   :: ActivationCode
                                   , serversRecordOwner            :: Owner }
                                   deriving (Show)

instance Record ServersRecord
instance ToJSON ServersRecord where
    toJSON (ServersRecord si co o) = object
        [ "Server" .= si
        , "Activation Code" .= co
        , "Owner" .= o ]
instance FromJSON ServersRecord where
    parseJSON (Object v) = do
        -- Important to take into consideration that all values come as arrays,
        -- so we must select the information on parse
        [si] <- v .:? "server" // [""]
        [co] <- v .:? "activation_code" // [""]
        [o]  <- v .:? "owner" // [""]
        return (ServersRecord si co o)


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

