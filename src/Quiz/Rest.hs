module Quiz.Rest
  ( api,
    server,
  )
where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Polysemy
import Polysemy.Error
import Quiz
import Quiz.Candidates
import Quiz.Effect.Randomize
import Quiz.Topics
import Servant
import Servant.JS

type API =
  "api" :> "questions" :> Get '[JSON] [Question]
    :<|> "api" :> "results" :> ReqBody '[JSON] (M.Map Topics Double) :> Post '[JSON] Results

api :: Proxy API
api = Proxy

server :: (Members '[Randomize, Error QuizError] r) => ServerT API (Sem r)
server = getQuestions :<|> matchUser

apiJS1 :: Text
apiJS1 = jsForAPI api vanillaJS
