module Quiz.Rest
  ( api,
    server,
  )
where

import qualified Data.Map.Strict as M
import Polysemy
import Quiz
import Quiz.Candidates
import Quiz.Effect.Randomize
import Quiz.Topics
import Servant

type API =
  "api" :> "questions" :> Get '[JSON] [Question]
    :<|> "api" :> "results" :> ReqBody '[JSON] (M.Map Topics Double) :> Post '[JSON] Results

api :: Proxy API
api = Proxy

server :: (Member Randomize r) => ServerT API (Sem r)
server = getQuestions :<|> matchUser
