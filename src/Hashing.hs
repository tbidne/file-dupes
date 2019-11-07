{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Hashing
( Hash(..)
, Data.Digest.Pure.SHA.Digest
, Data.Digest.Pure.SHA.SHA1State
) where

import Data.Digest.Pure.SHA
import Types

class Ord a => Hash a where
  hash :: FileSnip a -> a

instance Hash (Digest SHA1State) where
  hash :: FileSnip (Digest SHA1State) -> Digest SHA1State
  hash fs =
    let (_, bs) = unSnip fs
    in sha1 bs