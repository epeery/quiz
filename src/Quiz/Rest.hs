module Quiz.Rest
  ( api,
    server,
  )
where

import Data.Text (Text)
import Polysemy
import Polysemy.Error
import Quiz
import Quiz.Candidates
import Quiz.Topics (Question)
import Servant

type API =
  "api" :> "questions" :> Get '[JSON] [Question]
    :<|> "api" :> "results" :> Capture "responses" Text :> Get '[JSON] Results

api :: Proxy API
api = Proxy

server :: (Member (Error QuizError) r) => ServerT API (Sem r)
server = getQuestions :<|> matchUser
