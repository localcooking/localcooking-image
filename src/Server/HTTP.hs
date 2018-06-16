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

-- import LocalCooking.Database.Schema.Image (NextImageSource)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (forM_)
import Web.Routes.Nested (RouterT, matchHere, o_, textOnly)
import Network.Wai (requestHeaders, strictRequestBody)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd, FileInfo (..))
import Network.Wai.Trans (MiddlewareT)
import Network.HTTP.Types (status200)
-- import Database.Persist.Sql (ConnectionPool)



httpServer :: RouterT (MiddlewareT IO) sec IO ()
httpServer = do
  matchHere $ \app req resp -> do
    let hs = requestHeaders req
    print hs
    (_, files) <- parseRequestBody lbsBackEnd req -- FIXME use temporary dir
    print files
    forM_ files $ \(_, FileInfo{fileName,fileContent}) -> do
      pure ()
    resp (textOnly "yo" status200 [])
