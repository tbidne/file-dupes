module Config
( Env(..)
, nat
, unNat
, Nat) where

import Data.Text (Text)

newtype Nat = Nat { unNat :: Int }

nat :: Int -> Maybe Nat
nat n
  | n >= 0    = Just $ Nat n
  | otherwise = Nothing

data Env = Env
  { snipSz :: Nat,
    path :: Text
  }