module Main (main) where

import ListDir
import System.Directory.OsPath
import System.Environment

parseCliArgs :: [String] -> IO ()
parseCliArgs [] = listDir' Nothing =<< getCurrentDirectory
parseCliArgs [regex] = listDir' (Just regex) =<< getCurrentDirectory
parseCliArgs [regex, dir] = listDir (Just regex) dir
parseCliArgs _ = putStrLn "too many args bro wtf"

main :: IO ()
main = parseCliArgs =<< getArgs
