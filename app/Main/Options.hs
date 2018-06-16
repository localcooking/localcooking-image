{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , FlexibleContexts
  , RecordWildCards
  #-}


module Main.Options where

import Options.Applicative (Parser, execParser, info, helper, fullDesc, progDesc, header, strOption, option, switch, auto, long, help, value, showDefault)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.UTF8 as BS8
import Data.Monoid ((<>))
-- import Control.Logging (errorL, withStderrLogging)
import Control.Monad.Logger (runStderrLoggingT)
import Path (Path, Abs, Dir, parseAbsDir)
import System.Directory (doesDirectoryExist, createDirectory)
import System.Exit (exitFailure)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.Postgresql (createPostgresqlPool)



-- | Data type representing the raw arguments passed into the CLI executable
data ArgsImpl = ArgsImpl
  { argsImplPort       :: Int -- ^ Bound port of the server - @3000@
  , argsImplTarget     :: FilePath -- ^ Target directory for stored image uploads
  , argsImplDbHost     :: String -- ^ PostgreSQL host
  , argsImplDbPort     :: Int -- ^ PostgreSQL port
  , argsImplDbUser     :: String -- ^ PostgreSQL user
  , argsImplDbPassword :: String -- ^ PostgreSQL password
  , argsImplDbName     :: String -- ^ PostgreSQL database name
  }


-- | Arguments parser - marshalls CLI arguments into the 'ArgsImpl' data type.
args :: Parser ArgsImpl
args = ArgsImpl
    <$> parsePort
    <*> parseTarget
    <*> parseDbHost
    <*> parseDbPort
    <*> parseDbUser
    <*> parseDbPassword
    <*> parseDbName
  where
    parsePort = option auto $
      long "port" <> help "Bound port of the service"
        <> value 3000 <> showDefault
    parseTarget = strOption $
      long "target" <> help "Target directory for image uploads"
        <> value "/var/www/html/images/"
    parseDbHost = strOption $
      long "db-host" <> help "Hostname of the PostgreSQL database"
        <> value "localhost" <> showDefault
    parseDbPort = option auto $
      long "db-port" <> help "Port of the PostgreSQL database"
        <> value 5432 <> showDefault
    parseDbUser = strOption $
      long "db-user" <> help "User for the PostgreSQL database"
    parseDbPassword = strOption $
      long "db-password" <> help "Password for the PostgreSQL database"
    parseDbName = strOption $
      long "db-name" <> help "Database name for the PostgreSQL pooled connection"


execArgs :: ArgsImpl -> IO (ConnectionPool, Path Abs Dir)
execArgs ArgsImpl{..} = do
  case parseAbsDir argsImplTarget of
    Nothing -> do
      putStrLn "Can't parse target"
      exitFailure
    Just target -> do
      exists <- doesDirectoryExist argsImplTarget
      if not exists
        then do
          putStrLn "Target directory doesn't exist"
          exitFailure
        else do
          let connStr = T.encodeUtf8 $ T.unwords
                [ "host=" <> T.pack argsImplDbHost
                , "port=" <> T.pack (show argsImplDbPort)
                , "user=" <> T.pack argsImplDbUser
                , "password=" <> T.pack argsImplDbPassword
                , "dbname=" <> T.pack argsImplDbName
                ]
          backend <- runStderrLoggingT (createPostgresqlPool connStr 10)
          pure (backend, target)
