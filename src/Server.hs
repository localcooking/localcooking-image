{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , RankNTypes
  , RecordWildCards
  , FlexibleContexts
  #-}

module Server where

import Server.HTTP (httpServer)

import Path (Path, Abs, Dir)
import Web.Routes.Nested (RouterT, textOnly, route)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, MiddlewareT, runApplicationT)
import Network.HTTP.Types (status404)
import Database.Persist.Sql (ConnectionPool)



-- | Majority of business logic
server :: Int -- ^ Port to bind to
       -> ConnectionPool
       -> Path Abs Dir
       -> IO ()
server port backend target = do
  server' <- runApplicationT (route (httpServer backend target) defApp)
  runEnv port server'


-- | Simple @404@ response
defApp :: ApplicationT IO
defApp _ respond = respond (textOnly "404" status404 [])
