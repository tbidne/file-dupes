{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types
( FileSnip(..)
, DupMap(..)
, DupSearch(..)
, Hash(..)
, showMap
) where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

type DupMap a = Map a [Text]

showMap :: DupMap a -> Text
showMap mp = M.foldrWithKey f T.empty mp
  where f _ names acc
          | L.length names > 1 = (T.intercalate ", " names) `T.append` "\n\n" `T.append` acc
          | otherwise = acc

newtype FileSnip a = FileSnip { unSnip :: (Text, ByteString) }
  deriving Show

class Ord a => Hash a where
  hash :: FileSnip a -> a

class (Hash a, Show a, Monad m) => DupSearch m a where
  scan :: Text -> m [FileSnip a]

  mapify :: [FileSnip a] -> m (DupMap a)

  scanAndMapify :: Text -> m (DupMap a)
  scanAndMapify = scan >=> mapify