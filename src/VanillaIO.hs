{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VanillaIO
( vanilla
) where

import           Control.Monad
import           Data.ByteString.Lazy hiding (foldr)
import           Data.Map.Strict as Map hiding (foldr)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO

import Common
import Config
import Hashing
import Types

newtype VanillaIO a = VanillaIO { runVanilla :: IO a }
  deriving (Functor, Applicative, Monad)

vanilla :: Env -> IO Text
vanilla env = runVanilla $ do
  mp <- (scan >=> mapify) env
  return $ showMap mp

scan :: Env -> VanillaIO [FileSnip (Digest SHA1State)]
scan env = VanillaIO $ do
  files <- searchPath $ T.unpack $ path env
  sequence $ fmap (readSnip (snipSz env)) files

mapify :: [FileSnip (Digest SHA1State)] -> VanillaIO (DupMap (Digest SHA1State))
mapify = VanillaIO . return . foldr addSnipToMap Map.empty

readSnip :: Nat -> FilePath -> IO (FileSnip a)
readSnip n p = withFile p ReadMode $ \h -> do
  sp <- hGetNonBlocking h (unNat n)
  return $ FileSnip (T.pack p, sp)

addSnipToMap :: Hash a => FileSnip a -> DupMap a -> DupMap a
addSnipToMap fs mp =
  let (p, _) = unSnip fs
      digest = hash fs
  in case Map.lookup digest mp of
    Nothing    -> insert digest [p] mp
    Just files -> insert digest (p : files) mp