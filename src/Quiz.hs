{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Quiz
  ( Question,
    QuizError (..),
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
import GHC.Generics
import Polysemy
import Polysemy.Error
import Quiz.Candidates
import Quiz.Topics

data Question
  = Question
      { question :: Text,
        questionInfo :: Info,
        topic :: Text,
        id :: Topics
      }
  deriving (Generic)

instance FromJSON Question

instance ToJSON Question

data QuizError = ResponsesNotValid

getQuestions :: Sem r [Question]
getQuestions =
  return $
    ( \t ->
        Question
          { question = (getQuestion t),
            questionInfo = (getQuestionInfo t),
            Quiz.topic = (getTopic t),
            Quiz.id = t
          }
    )
      <$> topics
  where
    topics = getTopics @'[Education, Enviroment, Guns, Healthcare, Immigration]

matchUser :: (Member (Error QuizError) r) => Text -> Sem r Results
matchUser o = case decode (BSL.fromStrict . BS.pack $ T.unpack o) of
  Just a -> return $ matchUser' a
  Nothing -> throw ResponsesNotValid

matchUser' :: M.Map Topics Double -> Results
matchUser' = mostSimilarTo . makePerson
  where
    makePerson a = Respondant ("User" :: Text) a ""
