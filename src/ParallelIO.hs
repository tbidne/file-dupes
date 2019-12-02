{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module ParallelIO (ParallelIO(..)) where

import           Control.Parallel.Strategies
import qualified Data.Map.Strict as Map hiding (foldr)
import qualified Data.Text as T

import CommonIO
import Config
import Hashing
import DupScanner
import FileSnip

newtype ParallelIO a = ParallelIO { runParallel :: IO a }
  deriving (Functor, Applicative, Monad)

instance DupScanner ParallelIO where
  scan :: Env -> ParallelIO [FileSnip]
  scan Env{..} = ParallelIO $ scanIO path snipSz

  mapify :: [FileSnip] -> ParallelIO DupMap
  mapify fs = ParallelIO $
    let hs = parMap rseq (\x -> (hash x, x)) fs
    in return $ foldr ins (SHA1Map Map.empty) hs

  display :: DupMap -> ParallelIO ()
  display = ParallelIO . putStrLn . T.unpack . showMap

ins :: (SHA1, FileSnip) -> DupMap -> DupMap
ins (h, fs) (SHA1Map mp) =
    let (p, _) = unSnip fs
    in case Map.lookup h mp of
      Nothing    -> SHA1Map $ Map.insert h [p] mp
      Just files -> SHA1Map $ Map.insert h (p : files) mp