{-# LANGUAGE TemplateHaskell #-}

module Quiz.Effect.File
  ( File,
    deleteFile,
    readImage,
    runFile,
  )
where

import qualified Codec.Picture as P
import Codec.Picture (DynamicImage)
import Data.Text (Text, unpack)
import Polysemy
import Polysemy.Error
import Quiz (QuizError (..))
import qualified Turtle as T

data File m a where
  ReadImage :: Text -> File r DynamicImage
  DeleteFile :: Text -> File r ()

makeSem ''File

runFile :: (Members [Embed IO, Error QuizError] r) => Sem (File ': r) a -> Sem r a
runFile = interpret $ \case
  ReadImage p -> do
    i <- embed . P.readImage $ unpack p
    case i of
      Right pic -> return pic
      Left err -> throw $ PictureError err
  DeleteFile p -> embed . T.rm $ T.fromText p
