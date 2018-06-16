{-# LANGUAGE
    OverloadedStrings
  #-}

module ImageMagick.Command where

import Data.Image.Source (ImageSource)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (callCommand)
import Path (Path, Abs, Dir, toFilePath, parseRelFile, (</>))


-- | Builds shell command
magickConvert :: Path Abs Dir -- ^ Common directory
              -> FilePath -- ^ Input file
              -> ImageSource -- ^ Output file
              -> Maybe (IO ())
magickConvert prefix input' output'
  | not (isImageFileName input) = Nothing
  | otherwise =
    let cmd :: Text
        cmd = "convert "
            <> input
            <> " -resize '"
            <> T.pack (show totalArea)
            <> "@' "
            <> output
    in  Just $ callCommand $ T.unpack cmd
  where
    -- in pixels
    totalArea :: Int
    totalArea = 1000000

    stripSuffix :: Text -> Text
    stripSuffix = fst . T.breakOnEnd "."

    isImageFileName :: Text -> Bool
    isImageFileName x = T.length (stripSuffix x) >= 2

    input :: Text
    input = case parseRelFile input' of
      Just x -> T.pack $ toFilePath $ prefix </> x

    output :: Text
    output = case parseRelFile (show output' ++ ".png") of
      Just x -> T.pack $ toFilePath $ prefix </> x
