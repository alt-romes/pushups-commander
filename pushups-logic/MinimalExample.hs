{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
import Control.Monad (forM_)

data K
data L
data X
data Y
data CommonThing

thing :: Thing i CommonThing
thing = undefined

thingA :: Thing (Thing X CommonThing) Y
thingA = undefined

thingB :: Thing (Thing K CommonThing) L
thingB = undefined

class Constr a
class Constr2 a

data Thing a b = Thing { unThing :: a -> IO b }

runThings :: (Thing i o, Thing i' o) -> (Thing (Thing i o) b, Thing (Thing i' o) b') -> IO ()
runThings (x, x') (thing1, thing2) = forM_ ((thing1, x), (thing2, x')) (uncurry unThing)

-- What I want to do is just pass "thing" once to runThings
-- I've failed miserably and have been at it for a while ahah
main = runThings ( thing, thing ) ( thingA, thingB )

data SomeThing o where
    MkSomeThing :: (Constr2 o, Constr i) => Thing (Thing i CommonThing) o -> SomeThing o

-- Solution which took me way too long :)
runThings'' :: (forall a. Constr a => Thing a CommonThing) -> [SomeThing o] -> IO ()
runThings'' b = mapM_ (\(MkSomeThing thing) -> unThing thing b)
