{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Control.Monad.Except
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import PushupsCommander

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parsingTests, unitTests]

parsingTests :: TestTree
parsingTests = testGroup "Parsing (checked by QuickCheck)"
    [ QC.testProperty "Parse AddExercise" addEP
    , QC.testProperty "Parse RmExercise" remvEP
    ] where

    addEP :: Int -> Exercise -> String -> Property
    addEP amount exercise notes = (not (null notes) && amount > 0) ==>
        runExcept (parseMsg $ T.pack $ "+" <> show amount <> " " <> show exercise <> " " <> notes) == Right (AddExercise amount exercise $ T.pack (dropWhile (== ' ') notes))

    remvEP :: Int -> Exercise -> String -> Property
    remvEP amount exercise notes = (not (null notes) && amount > 0) ==>
        runExcept (parseMsg $ T.pack $ "-" <> show amount <> " " <> show exercise <> " " <> notes) == Right (RmExercise amount exercise $ T.pack (dropWhile (== ' ') notes))

instance Arbitrary Exercise where
    arbitrary = elements [Pushups, Abs, Squats, Kilometers]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Parse +1" addE
    ] where

    addE :: Assertion
    addE = runExcept (parseMsg "+1") @?= Right (AddExercise 1 Pushups "")

    
