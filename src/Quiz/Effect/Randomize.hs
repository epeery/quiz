{-# LANGUAGE TemplateHaskell #-}

module Quiz.Effect.Randomize
  ( Randomize,
    randomize,
    runRandomize,
  )
where

import Polysemy
import Polysemy.Random

data Randomize m a where
  Randomize :: [b] -> Randomize r [b]

makeSem ''Randomize

runRandomize :: (Member Random r) => Sem (Randomize ': r) a -> Sem r a
runRandomize = interpret $ \case
  Randomize xs -> shuffle xs

shuffle :: (Member Random r) => [a] -> Sem r [a]
shuffle x =
  if length x < 2
    then return x
    else do
      i <- randomR (0, length (x) -1)
      r <- shuffle (take i x ++ drop (i + 1) x)
      return (x !! i : r)
