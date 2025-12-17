module Main (main) where

import Config
import Control.Exception
import ListDirs
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  cfg <- either throwIO pure $ parseCliArgs args
  listDirs cfg
