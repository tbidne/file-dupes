{-# LANGUAGE InstanceSigs #-}

module Arbitraries (Arbitrary(..)) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Test.QuickCheck

import FileSnip
import Hashing

instance Arbitrary B.ByteString where
  arbitrary :: Gen B.ByteString
  arbitrary = fmap B.pack arbitrary

instance Arbitrary T.Text where
  arbitrary :: Gen T.Text
  arbitrary = fmap (T.pack . getPrintableString) (arbitrary :: Gen PrintableString)

instance Arbitrary FileSnip where
  arbitrary :: Gen FileSnip
  arbitrary = fmap FileSnip $ (,) <$> arbitrary <*> arbitrary

instance Arbitrary DupMap where
  arbitrary :: Gen DupMap
  arbitrary = do
    fs <- listOf arbitrary
    let mp = foldr addSnipToMap (SHA1Map M.empty) fs
    return mp