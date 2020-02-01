{-# LANGUAGE OverloadedStrings #-}

module Quiz.Results
  ( results,
  )
where

import Quiz.Candidates
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

results :: Results -> Html
results _ = docTypeHtml $ do
  H.head $ do
    H.title "My 2020 Lineup"
  body $ do
    H.div ! class_ "container" $ do
      h1 "Whoop!"
