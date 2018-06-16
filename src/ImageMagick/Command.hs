{-# LANGUAGE
    OverloadedStrings
  #-}

module ImageMagick.Command where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (CreateProcess, shell)


-- | Builds shell command
magickConvert :: FilePath -- ^ Input file
              -> Maybe CreateProcess
magickConvert input'
  | not (isImageFileName input) = Nothing
  | otherwise =
    let cmd :: Text
        cmd = "magick convert "
            <> input
            <> " -resize '"
            <> T.pack (show totalArea)
            <> "@' "
            <> stripSuffix input
            <> "png"
    in  Just $ shell $ T.unpack cmd
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
