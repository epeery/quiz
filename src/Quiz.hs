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
import Data.Maybe (maybe)
import Data.Text (Text)
import Polysemy
import Quiz.Candidates (Respondant (..), Results, mostSimilarTo)
import Quiz.Effect.Randomize
import Quiz.Topics

data QuizError
  = ResponsesNotValid
  | PictureError String
  | FailedToStartProcess

getQuestions :: (Member Randomize r) => Sem r [Question]
getQuestions = randomize questions

matchUser :: [(String, Double)] -> Sem r Results
matchUser o = return . matchUser' . M.fromList $ o'
  where
    -- Attempts to convert the keys into topics. Leaves out any that fail.
    o' = foldr (\(t, d) acc -> maybe acc (\x -> (x, d) : acc) (readTopic t)) [] o

matchUser' :: M.Map Topics Double -> Results
matchUser' = mostSimilarTo . makePerson
  where
    makePerson a = Respondant ("User" :: Text) a ""
