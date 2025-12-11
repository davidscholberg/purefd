module Main (main) where

import ListDir
import System.Environment

parseCliArgs :: [String] -> IO ()
parseCliArgs [] = listDir Nothing ""
parseCliArgs [regex] = listDir (Just regex) ""
parseCliArgs [regex, dir] = listDir (Just regex) dir
parseCliArgs _ = putStrLn "too many args bro wtf"

main :: IO ()
main = parseCliArgs =<< getArgs
