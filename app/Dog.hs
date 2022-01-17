{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as BS

import Data.Aeson -- JSON
import Data.Text
import RecordM

newtype Owner = Owner Text
data Dog = Dog (Ref Owner) Text deriving (Show)

instance Record Owner where
    definition = "Owners" 

instance Record Dog where
    definition = "Dogs" 

businessLogic :: Cob [(Ref Dog, Dog)]
businessLogic = do
    ownerId <- rmAddInstance (Owner "rodrigo")
    rmAddInstance (Dog ownerId "bobby")
    dogs <- rmDefinitionSearch defaultRMQuery
    forM_ dogs (rmAddInstance . snd)
    return dogs

main :: IO ()
main = do
    host    <- BS.init <$> BS.readFile "cob-host.secret"
    token   <- BS.init <$> BS.readFile "cob-token.secret"
    session <- makeSession host token
    dogs <- runCobT session businessLogic
    forM_ dogs print










































-- instance Record Owner where
--     definition = "Owner"

-- cob :: Cob ()
-- cob = do
--   rmAddInstance (Owner "rodrigo")
--   return ()


-- main :: IO ()
-- main = do
--     host    <- BS.init <$> BS.readFile "cob-host.secret"
--     token   <- BS.init <$> BS.readFile "cob-token.secret"
--     session <- makeSession host token
--     runCobT session cob
--     return ()


instance FromJSON Owner where
    parseJSON = withObject "owner" $ \v -> do
        [owner] <- v .: "owner"
        return (Owner owner)
instance FromJSON Dog where
    parseJSON = withObject "dog" $ \v -> do
        [ownerId] <- v .: "owner"
        [dog] <- v .: "dog"
        return (Dog (Ref $ read ownerId) dog)
instance ToJSON Owner where
    toJSON (Owner n) = object
        [ "Owner" .= n ]
instance ToJSON Dog where
    toJSON (Dog (Ref ownerId) dog) = object
        [ "Owner" .= show ownerId
        , "Dog"   .= dog ]

