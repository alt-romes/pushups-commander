module PushupsCommander where

type Amount = Int
data Exercise = Pushups | Abs | Squats | Kilometers
data ServerPlan = Small | Medium | Large | Huge
instance Show Exercise
instance Show ServerPlan
