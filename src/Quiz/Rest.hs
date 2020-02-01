module Quiz.Rest
  ( api,
    server,
  )
where

import Codec.Picture
import qualified Data.Map.Strict as M
import Polysemy
import Quiz
import Quiz.Candidates
import Quiz.Effect.Picture
import Quiz.Effect.Randomize
import Quiz.Topics
import Servant
import Servant.JuicyPixels

type API =
  "api" :> "questions" :> Get '[JSON] [Question]
    :<|> "api" :> "results" :> ReqBody '[JSON] (M.Map Topics Double) :> Post '[JSON] Results
    :<|> "api" :> "image" :> ReqBody '[JSON] Results :> Post '[PNG] DynamicImage

api :: Proxy API
api = Proxy

server :: (Members '[Randomize, Picture] r) => ServerT API (Sem r)
server = getQuestions :<|> matchUser :<|> getPicture
