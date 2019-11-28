{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module VanillaIO (VanillaIO(..)) where

import           Data.ByteString.Lazy hiding (foldr, putStrLn)
import           Data.Map.Strict as Map hiding (foldr)
import qualified Data.Text as T
import           System.IO

import Common
import Config
import Hashing
import DupScanner
import FileSnip

newtype VanillaIO a = VanillaIO { runVanilla :: IO a }
  deriving (Functor, Applicative, Monad)

instance DupScanner VanillaIO where
  scan :: Env -> VanillaIO [FileSnip]
  scan Env{..} = VanillaIO $ do
    files <- searchPath $ T.unpack path
    traverse (readSnip snipSz) files

  mapify :: [FileSnip] -> VanillaIO DupMap
  mapify = VanillaIO . return . foldr addSnipToMap (SHA1Map Map.empty)

  display :: DupMap -> VanillaIO ()
  display = VanillaIO . putStrLn . T.unpack . showMap

readSnip :: Nat -> FilePath -> IO FileSnip
readSnip n p = withFile p ReadMode $ \h -> do
  sp <- hGetNonBlocking h (unNat n)
  return $ FileSnip (T.pack p, sp)