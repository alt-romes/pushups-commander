{-# LANGUAGE TemplateHaskellQuotes #-}
module Utils where

import Language.Haskell.TH

replicateTuple :: Int -> a -> Q Exp
replicateTuple n x = TupE . replicate n <$> (Just <$> [e| ex |])
