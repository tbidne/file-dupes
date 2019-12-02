{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module VanillaIO (VanillaIO(..)) where

import           Data.Map.Strict as Map hiding (foldr)
import qualified Data.Text as T

import CommonIO
import Config
import Hashing
import DupScanner
import FileSnip

newtype VanillaIO a = VanillaIO { runVanilla :: IO a }
  deriving (Functor, Applicative, Monad)

instance DupScanner VanillaIO where
  scan :: Env -> VanillaIO [FileSnip]
  scan Env{..} = VanillaIO $ scanIO path snipSz

  mapify :: [FileSnip] -> VanillaIO DupMap
  mapify = VanillaIO . return . foldr addSnipToMap (SHA1Map Map.empty)

  display :: DupMap -> VanillaIO ()
  display = VanillaIO . putStrLn . T.unpack . showMap