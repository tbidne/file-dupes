module ConfigSpec (spec) where

import Data.Maybe
import Test.Hspec
import Test.Hspec.QuickCheck

import Config

spec :: Spec
spec = do
  describe "Config Tests" $ do
    prop "Verifying nat totality" vNat

vNat :: Int -> Bool
vNat n
  | n < 0     = isNothing $ nat n
  | otherwise = isJust    $ nat n