{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module VanillaIO
( scan
, mapify
, vanilla
) where

import           Data.ByteString.Lazy hiding (foldr)
import           Data.Digest.Pure.SHA
import           Data.Map.Strict as Map hiding (foldr)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO

import Common
import Types

type VanillaIO = IO

vanilla :: Text -> IO Text
vanilla s = do
  mp <- scanAndMapify s :: VanillaIO (DupMap (Digest SHA1State))
  return $ showMap mp

instance Hash (Digest SHA1State) where
  hash :: FileSnip (Digest SHA1State) -> Digest SHA1State
  hash fs =
    let (_, bs) = unSnip fs
    in sha1 bs

instance DupSearch VanillaIO (Digest SHA1State) where
  scan :: Text -> VanillaIO [FileSnip (Digest SHA1State)]
  scan root = do
    files <- searchPath $ T.unpack root
    sequence $ fmap readSnip files

  mapify :: [FileSnip (Digest SHA1State)] -> VanillaIO (DupMap (Digest SHA1State))
  mapify = return . foldr addSnipToMap Map.empty

readSnip :: FilePath -> VanillaIO (FileSnip a)
readSnip p = withFile p ReadMode $ \h -> do
  sp <- hGetNonBlocking h 1024
  return $ FileSnip (T.pack p, sp)

addSnipToMap :: (Ord a, Hash a) => FileSnip a -> DupMap a -> DupMap a
addSnipToMap fs mp =
  let (p, _) = unSnip fs
      digest = hash fs
  in case Map.lookup digest mp of
    Nothing    -> insert digest [p] mp
    Just files -> insert digest (p : files) mp