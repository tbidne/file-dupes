{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Hashing
( Hash(..)
, SHA1
, addSnipToMap
) where

import Data.Digest.Pure.SHA
import Data.Map.Strict
import Prelude hiding (lookup)

import DupMap
import FileSnip

type SHA1 = Digest SHA1State

class Ord a => Hash a where
  hash :: FileSnip a -> a

instance Hash SHA1 where
  hash :: FileSnip SHA1 -> SHA1
  hash fs =
    let (_, bs) = unSnip fs
    in sha1 bs

addSnipToMap :: Hash a => FileSnip a -> DupMap a -> DupMap a
addSnipToMap fs mp =
  let (p, _) = unSnip fs
      digest = hash fs
  in case lookup digest mp of
    Nothing    -> insert digest [p] mp
    Just files -> insert digest (p : files) mp