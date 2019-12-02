{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Hashing
( Hash(..)
, DupMap(..)
, SHA1
, addSnipToMap
, memberSnip
, showMap
) where

import           Data.Digest.Pure.SHA
import           Data.Map.Strict
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (lookup)

import FileSnip

type SHA1 = Digest SHA1State

class Ord a => Hash a where
  hash :: FileSnip -> a

instance Hash SHA1 where
  hash :: FileSnip -> SHA1
  hash = sha1 . snd . unSnip

data DupMap where
  SHA1Map :: Map SHA1 [Text] -> DupMap

instance Show DupMap where
  show = T.unpack . showMap

addSnipToMap :: FileSnip -> DupMap -> DupMap
addSnipToMap fs (SHA1Map mp) = SHA1Map $ insMap fs mp

memberSnip :: FileSnip -> DupMap -> Bool
memberSnip fs (SHA1Map mp) =
  let digest = hash fs
  in member digest mp

insMap :: Hash a => FileSnip -> Map a [Text] -> Map a [Text]
insMap fs mp =
  let (p, _) = unSnip fs
      digest = hash fs
  in case lookup digest mp of
    Nothing    -> insert digest [p] mp
    Just files -> insert digest (p : files) mp

showMap :: DupMap -> Text
showMap (SHA1Map mp) = showRaw mp

showRaw :: Show a => (Map a [Text]) -> Text
showRaw = foldrWithKey f ""
  where f k names acc = case names of
          [] -> acc
          xs -> "Key: " <> (T.pack . show) k <> "\n" <> showNames xs acc

showNames :: [Text] -> Text -> Text
showNames names acc = (T.intercalate "\n" names) <> "\n\n" <> acc