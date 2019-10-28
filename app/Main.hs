module Main where

import Control.Monad
import Data.Text
import System.Environment  

import VanillaIO

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing     -> putStrLn "Usage: stack exec file-dupes-exe <path>"
    Just path   -> packAndRun vanilla path

parseArgs :: [String] -> Maybe String
parseArgs [path] = Just path
parseArgs _ = Nothing

packAndRun :: (Text -> IO Text) -> String -> IO ()
packAndRun f = f . pack >=> putStrLn . unpack