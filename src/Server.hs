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

import Web.Routes.Nested (RouterT, textOnly, route)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, MiddlewareT, runApplicationT)
import Network.HTTP.Types (status404)



-- | Majority of business logic
server :: Int -- ^ Port to bind to
       -> IO ()
server port = do
  server' <- runApplicationT (route httpServer defApp)
  runEnv port server'


-- | Simple @404@ response
defApp :: ApplicationT IO
defApp _ respond = respond (textOnly "404" status404 [])
