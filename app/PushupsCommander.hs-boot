module PushupsCommander where

import Data.Text

type Amount = Int
data Exercise = Pushups | Abs | Squats | Kilometers | Unknown Text
instance Show Exercise
