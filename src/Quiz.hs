{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Quiz
  ( QuizError (..),
    getQuestions,
    matchUser,
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Quiz.Candidates (Respondant (..), Results, mostSimilarTo)
import Quiz.Effect.Randomize
import Quiz.Topics

data QuizError = ResponsesNotValid

getQuestions :: (Member Randomize r) => Sem r [Question]
getQuestions = randomize questions

-- matchUser :: (Member (Error QuizError) r) => Text -> Sem r Results
-- matchUser o = case decode (BSL.fromStrict . BS.pack $ T.unpack o) of
--   Just a -> return $ matchUser' a
--   Nothing -> throw ResponsesNotValid

matchUser :: (Member (Error QuizError) r) => M.Map Topics Double -> Sem r Results
matchUser o = return $ matchUser' o

matchUser' :: M.Map Topics Double -> Results
matchUser' = mostSimilarTo . makePerson
  where
    makePerson a = Respondant ("User" :: Text) a ""
