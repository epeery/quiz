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
import Quiz.Topics

data QuizError = ResponsesNotValid

getQuestions :: Sem r [Question]
getQuestions = return $ questions

matchUser :: (Member (Error QuizError) r) => Text -> Sem r Results
matchUser o = case decode (BSL.fromStrict . BS.pack $ T.unpack o) of
  Just a -> return $ matchUser' a
  Nothing -> throw ResponsesNotValid

matchUser' :: M.Map Topics Double -> Results
matchUser' = mostSimilarTo . makePerson
  where
    makePerson a = Respondant ("User" :: Text) a ""
