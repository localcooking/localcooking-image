{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , RecordWildCards
  , DataKinds
  , QuasiQuotes
  #-}

module Server.HTTP where

import ImageMagick.Command (magickConvert)
import LocalCooking.Database.Query.Image (nextImageSource)

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Path (Path, Abs, Dir, toFilePath)
import Control.Monad (forM_)
import Web.Routes.Nested (RouterT, matchHere, o_, textOnly, jsonOnly)
import Network.Wai (requestHeaders, strictRequestBody)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, FileInfo (..))
import Network.Wai.Trans (MiddlewareT)
import Network.HTTP.Types (status200, status400, status404)
import Database.Persist.Sql (ConnectionPool)



httpServer :: ConnectionPool -> Path Abs Dir -> RouterT (MiddlewareT IO) sec IO ()
httpServer backend target = do
  matchHere $ \app req resp -> do
    let hs = requestHeaders req
    print hs
    (_, files) <- parseRequestBody lbsBackEnd req -- FIXME use temporary dir
    case files of
      ((_,FileInfo{fileName,fileContent}):_) -> do
        let fileName' :: FilePath
            fileName' = BS8.toString fileName
        output <- nextImageSource backend
        case magickConvert fileName' output of
          Nothing -> resp (textOnly "" status404 []) -- shit, wasted a filename
          Just runImageMagick -> do
            LBS.writeFile fileName' fileContent
            runImageMagick
            resp (jsonOnly output status200 [])
      _ -> resp (textOnly "" status400 [])
