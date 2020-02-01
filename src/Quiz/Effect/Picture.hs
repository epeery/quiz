{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Quiz.Effect.Picture
  ( Picture,
    getPicture,
    runPicture,
  )
where

import Codec.Picture (DynamicImage)
import Data.UUID (toText)
import Polysemy
import Quiz.Candidates (Results)
import Quiz.Effect.File
import Quiz.Effect.Process
import Quiz.Effect.UUID
import Quiz.Results

data Picture m a where
  GetPicture :: Results -> Picture r DynamicImage

makeSem ''Picture

runPicture :: (Members [UUID, Process, File] r) => Sem (Picture ': r) a -> Sem r a
runPicture = interpret $ \case
  GetPicture r -> do
    uuid <- getUUID
    let html = results r
        p = "node ./results/index.js '" <> html <> "' " <> "./pictures/ " <> toText uuid
    _ <- createProcess p
    img <- readImage $ "./pictures/" <> toText uuid <> ".png"
    -- deleteFile $ imgPath <> ".png"
    return img
