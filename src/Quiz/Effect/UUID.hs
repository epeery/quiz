{-# LANGUAGE TemplateHaskell #-}

module Quiz.Effect.UUID
  ( UUID,
    getUUID,
    runUUID,
  )
where

import qualified Data.UUID as U
import Data.UUID.V4 (nextRandom)
import Polysemy

data UUID m a where
  GetUUID :: UUID r U.UUID

makeSem ''UUID

runUUID :: (Member (Embed IO) r) => Sem (UUID ': r) a -> Sem r a
runUUID = interpret $ \case
  GetUUID -> embed nextRandom
