{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Quiz
  ( QuizError (..),
    getQuestions,
    matchUser,
  )
where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Polysemy
import Quiz.Candidates (Respondant (..), Results, mostSimilarTo)
import Quiz.Effect.Randomize
import Quiz.Topics

data QuizError = ResponsesNotValid

getQuestions :: (Member Randomize r) => Sem r [Question]
getQuestions = randomize questions

matchUser :: M.Map Topics Double -> Sem r Results
matchUser o = return $ matchUser' o

matchUser' :: M.Map Topics Double -> Results
matchUser' = mostSimilarTo . makePerson
  where
    makePerson a = Respondant ("User" :: Text) a ""
