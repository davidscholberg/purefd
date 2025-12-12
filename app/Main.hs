module Main (main) where

import Config
import Control.Exception
import ListDir
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  cfg <- either throwIO pure $ parseCliArgs args
  listDir cfg
