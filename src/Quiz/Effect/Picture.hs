{-# LANGUAGE TemplateHaskell #-}

module Quiz.Effect.Picture
  ( Picture,
    getPicture,
    runPicture,
  )
where

import Codec.Picture (DynamicImage, readImage)
import Debug.Trace (trace)
import Polysemy
import Polysemy.Error
import Quiz (QuizError (..))
import Quiz.Candidates (Results)

data Picture m a where
  GetPicture :: Results -> Picture r DynamicImage

makeSem ''Picture

runPicture :: (Members [Embed IO, Error QuizError] r) => Sem (Picture ': r) a -> Sem r a
runPicture = interpret $ \case
  GetPicture _ -> do
    i <- embed $ readImage "./joe.png"
    case i of
      Left s -> trace s $ throw PictureNotFound
      Right pic -> return pic
