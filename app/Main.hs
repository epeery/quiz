{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Control.Monad.Except
import Data.Function ((&))
import GHC.Generics
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors (simpleCors)
import Options.Generic
import Polysemy
import Polysemy.Error
import Polysemy.Random
import Quiz
import Quiz.Effect.Randomize
import Quiz.Rest
import Servant.Server

createApp :: IO Application
createApp = return (serve api $ hoistServer api interpretServer server)
  where
    interpretServer sem =
      sem
        & runError @QuizError
        & runRandomize
        & runRandomIO
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ResponsesNotValid)) = Left err404 {errBody = "Malformed responses"}
    handleErrors (Right value) = Right value

data Args = Args {port :: Int} deriving (Generic, Show)

instance ParseRecord Args

main :: IO ()
main = do
  Args (port') <- getRecord "Quiz API Server"
  app <- createApp
  W.run port' . simpleCors $ app
