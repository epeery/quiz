{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Quiz.Results
  ( results,
  )
where

import Control.Monad (forM_)
import Data.Text
import Quiz.Candidates
import Text.Blaze.Html.Renderer.String as S
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

results :: Results -> Text
results = pack . renderHtml . results'

results' :: Results -> Html
results' rs = do
  H.div ! class_ "Results" $ do
    H.div ! class_ "candidates" $ do
      forM_ rs candidateContainer

candidateContainer :: (Candidate, Double) -> Html
candidateContainer (Respondant {..}, d) = do
  let rounded = fromIntegral ((round (10 * d * 100)) :: Int) / 10
  H.div ! class_ "candidate-container" $ do
    H.div ! class_ "candidate-pic-container small" $ do
      img
        ! class_ "candidate-pic small"
        ! src (textValue rPic)
    H.div ! class_ "candidate-ranking" $ do
      H.div ! class_ "candidate-ranking-text" $ do
        h2 . toHtml $ rName
        h2 . toHtml $ showPercent rounded
      H.div
        ! class_ ("candidate-match-bar " <> color rounded)
        ! A.style (textValue $ "width: " <> showPercent rounded)
        $ return ()
  where
    color percent
      | percent > 70 = "blue1"
      | percent > 50 = "blue2"
      | percent > 40 = "pink1"
      | otherwise = "pink2"

showPercent :: Double -> Text
showPercent d =
  let s =
        if isInt d
          then pack $ show (round d :: Int)
          else pack $ show d
   in s <> "%"

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)
