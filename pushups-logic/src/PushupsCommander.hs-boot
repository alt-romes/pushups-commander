module PushupsCommander where

type Amount = Int
data Exercise = Pushups | Abs | Squats | Kilometers
data ServerPlan = Small | Medium | Large | Huge
data TimeInterval = Day | Week | Month | Year
instance Show Exercise
instance Show ServerPlan
instance Show TimeInterval
