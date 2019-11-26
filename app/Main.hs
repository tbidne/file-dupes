module Main where

import Control.Monad
import Control.Monad.Reader
import Data.Text
import System.Environment  

import Config
import Hashing
import DupScanner
import VanillaIO

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing       -> putStrLn "Usage: stack exec file-dupes-exe <path> <snip-size> <v|c|p>"
    Just (f, env) -> f env

parseArgs :: [String] -> Maybe ((Env -> IO ()), Env)
parseArgs [p]        = (,) <$> Just vanilla      <*> parseEnv p 3000000
parseArgs [p, sz]    = (,) <$> Just vanilla      <*> parseEnv p (read sz :: Int)
parseArgs [p, sz, t] = (,) <$> parseSearchType t <*> parseEnv p (read sz :: Int)
parseArgs _          = Nothing

parseSearchType :: String -> Maybe (Env -> IO ())
parseSearchType "v" = Just vanilla
parseSearchType _   = Nothing

parseEnv :: String -> Int -> Maybe Env
parseEnv p sz =
  case nat sz of
    Just n  -> Just $ Env n (pack p)
    Nothing -> Nothing

vanilla :: Env -> IO ()
vanilla = runVanilla . (display <=< runReaderT f)
  where f :: ReaderT Env VanillaIO DupMap
        f = entry