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
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Generic
import Polysemy
import Polysemy.Error
import Polysemy.Random
import Quiz
import Quiz.Effect.Picture
import Quiz.Effect.Randomize
import Quiz.Rest
import Servant.Server

createApp :: IO Application
createApp = return (serve api $ hoistServer api interpretServer server)
  where
    interpretServer sem =
      sem
        & runRandomize
        & runRandomIO
        & runPicture
        & runError @QuizError
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ResponsesNotValid)) = Left err404 {errBody = "Malformed responses"}
    handleErrors (Left (PictureNotFound)) = Left err404 {errBody = "Picture not found"}
    handleErrors (Right value) = Right value

data Args = Args {port :: Int} deriving (Generic, Show)

instance ParseRecord Args

myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"],
          corsMethods = "PUT" : simpleMethods
        }

main :: IO ()
main = do
  Args (port') <- getRecord "Quiz API Server"
  app <- createApp
  W.run port' . myCors $ app
