{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import System.Random
import Data.Function ((&))
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Cob
import Cob.RecordM
import Cob.RecordM.Testing
import PushupsCommander
import PushupsRecordM

main = do
    host     <- init <$> readFile "cob-host.secret"
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession host cobToken

    defaultMain (tests session)

tests :: CobSession -> TestTree
tests session = testGroup "Tests" [parsingTests, unitTests session]

parsingTests :: TestTree
parsingTests = testGroup "Parsing (checked by QuickCheck)"
    [ QC.testProperty "Parse AddExercise" addEP
    , QC.testProperty "Parse RmExercise" remvEP
    ] where

    addEP :: Int -> Exercise -> String -> Property
    addEP amount exercise notes = (not (null notes) && amount > 0) ==>
        runExceptT (runParser parseMsg $ T.pack $ "+" <> show amount <> " " <> show exercise <> " " <> notes) == Just (Right (AddExercise (fromIntegral amount) exercise $ T.pack (dropWhile (== ' ') notes)))

    remvEP :: Int -> Exercise -> String -> Property
    remvEP amount exercise notes = (not (null notes) && amount > 0) ==>
        runExceptT (runParser parseMsg $ T.pack $ "-" <> show amount <> " " <> show exercise <> " " <> notes) == Just (Right (RmExercise (fromIntegral amount) exercise $ T.pack (dropWhile (== ' ') notes)))

instance Arbitrary Exercise where
    arbitrary = elements [Pushups, Abs, Squats, Kilometers]


unitTests :: CobSession -> TestTree
unitTests session = testGroup "Pushups Bot Unit Tests"
    [ testCase "Activate Commander" (runPushupsTest session test_activateCommander)
    , testCase "Add Exercise" (runPushupsTest session test_addExercise)
    , testCase "Set Master Username" (runPushupsTest session test_setMasterUsername)
    , testCase "Set Profile Picture" (runPushupsTest session test_setProfilePic)
    , testCase "Linking" (runPushupsTest session test_linking)
    , testCase "Create Challenge" (runPushupsTest session test_createChallenge)
    ]

deriving instance Show ExercisesRecord

runPushupsTest :: CobSession -> PushupsBotM IO a -> IO a
runPushupsTest session test = either assertFailure return =<< do
    i <- randomIO @Int
    runRecordMTests session (runReaderT test ("teste-server" <> T.pack (show i), "teste-serveruser" <> T.pack (show i)))

test_addExercise :: PushupsBotM IO ()
test_addExercise = do
    test_activateCommander
    threadDelay 1000000            & liftIO
    i' <- randomRIO @Int (1, 201)   & liftIO
    let ii = fromIntegral i'
    addExercise ii Pushups "test user note"
    addExercise ii Abs "test user note2"
    addExercise ii Squats "test user note3"
    i <- randomRIO @Float (1, 21)   & liftIO
    addExercise i Kilometers "test user note5"
    return ()

test_activateCommander :: PushupsBotM IO ()
test_activateCommander = do
    serverId <- asks fst
    ref <- rmAddInstanceSync (ServersRecord Nothing serverId serverId Medium) & lift
    ServersRecord (Just sId) _ _ Medium <- activateCommander serverId
    (serverId @=? sId) & liftIO
    return ()

test_setMasterUsername :: PushupsBotM IO ()
test_setMasterUsername = do
    test_activateCommander
    threadDelay 1000000            & liftIO
    i <- fromIntegral <$> randomRIO @Int (1, 201)   & liftIO
    (serverId, serverUserId) <- ask
    setMasterUsername ("newname" <> T.pack (show i))
    setMasterUsername ("newnameagain" <> T.pack (show i))
    threadDelay 1000000            & liftIO
    [ServerUsersRecord userId _ _] <- rmDefinitionSearch_ ("server_reference:" <> serverId <> " server_username:" <> serverUserId) & lift
    UsersRecord nwName _ _ <- rmGetInstance userId & lift
    (("newnameagain" <> T.pack (show i)) @=? nwName) & liftIO
    return ()

test_setProfilePic :: PushupsBotM IO ()
test_setProfilePic = do
    test_activateCommander
    threadDelay 1000000            & liftIO
    i <- randomRIO @Int (1, 201)   & liftIO
    setProfilePic ("htf://fakeurl/" <> T.pack (show i))
    setProfilePic ("htf://fakeurl2/" <> T.pack (show i))
    setProfilePic ("htf://fakeurl3/" <> T.pack (show i))
    return ()

test_linking :: PushupsBotM IO ()
test_linking = do
    test_activateCommander
    session <- lift ask
    i <- randomIO @Int
    code <- lift $ flip runReaderT ("teste-server" <> T.pack (show i), "teste-serveruser" <> T.pack (show i)) $ do
                test_activateCommander
                threadDelay 1000000 & liftIO
                getLinkingCode -- get linking code from user in another server
    threadDelay 1000000 & liftIO
    linkMasterUsername code
    code2 <- getLinkingCode
    threadDelay 1000000 & liftIO
    linkMasterUsername code2
    return ()

test_createChallenge :: PushupsBotM IO ()
test_createChallenge = do
    test_activateCommander
    threadDelay 1000000 & liftIO
    session <- lift ask
    i <- randomIO
    createChallenge i Pushups Daily (Just "Comment")
    return ()


