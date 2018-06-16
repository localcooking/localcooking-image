{-# LANGUAGE
    OverloadedStrings
  #-}

module ImageMagick.Command where

import Data.Image.Source (ImageSource)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (callCommand)


-- | Builds shell command
magickConvert :: FilePath -- ^ Input file
              -> ImageSource -- ^ Output file
              -> Maybe (IO ())
magickConvert input' output'
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
    input = T.pack input'

    output :: Text
    output = T.pack (show output') <> ".png"
