{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VanillaIO (VanillaIO(..)) where

import           Data.ByteString.Lazy hiding (foldr, putStrLn)
import           Data.Map.Strict as Map hiding (foldr)
import qualified Data.Text as T
import           System.IO

import Common
import Config
import Hashing
import DupMap
import DupScanner
import FileSnip

newtype VanillaIO a = VanillaIO { runVanilla :: IO a }
  deriving (Functor, Applicative, Monad)

instance DupScanner VanillaIO SHA1 where
  scan :: Env -> VanillaIO [FileSnip SHA1]
  scan env = VanillaIO $ do
    files <- searchPath $ T.unpack $ path env
    sequenceA $ fmap (readSnip (snipSz env)) files

  mapify :: [FileSnip SHA1] -> VanillaIO (DupMap SHA1)
  mapify = VanillaIO . return . foldr addSnipToMap Map.empty

  display :: DupMap SHA1 -> VanillaIO ()
  display mp = VanillaIO $ (putStrLn . T.unpack . showMap) mp

readSnip :: Nat -> FilePath -> IO (FileSnip a)
readSnip n p = withFile p ReadMode $ \h -> do
  sp <- hGetNonBlocking h (unNat n)
  return $ FileSnip (T.pack p, sp)