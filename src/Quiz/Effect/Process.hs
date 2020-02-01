{-# LANGUAGE TemplateHaskell #-}

module Quiz.Effect.Process
  ( Process,
    createProcess,
    runProcess,
  )
where

import Polysemy
import Polysemy.Error
import Quiz
import Turtle

data Process m a where
  CreateProcess :: Text -> Process r ()

makeSem ''Process

runProcess :: (Members [Error QuizError, Embed IO] r) => Sem (Process ': r) a -> Sem r a
runProcess = interpret $ \case
  CreateProcess p -> do
    ec <- embed $ shell p empty
    case ec of
      ExitFailure _ -> throw FailedToStartProcess
      ExitSuccess -> return ()
