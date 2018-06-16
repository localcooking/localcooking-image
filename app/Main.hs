module Main where

import Main.Options (execArgs, args)
import Server (server)

import Data.Monoid ((<>))
import Options.Applicative (Parser, execParser, info, helper, fullDesc, progDesc, header, strOption, option, switch, auto, long, help, value, showDefault)



main :: IO ()
main = do
  cliArgs <- execParser opts
  (backend, target) <- execArgs cliArgs
  server 3000
  where
    opts = info (helper <*> args) $ fullDesc <> progDesc desc <> header head'
    desc = "Start the daemon"
    head' = "Local Cooking Images"
