{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DupScanner
( DupScanner(..)
, entry
, showMap) where

import           Control.Monad.Reader
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import Config
import DupMap
import FileSnip
import Hashing

class (Hash a, Monad m) => DupScanner m a where
  scan :: Env -> m [FileSnip a]
  mapify :: [FileSnip a] -> m (DupMap a)
  display :: (Map a [Text]) -> m ()

instance DupScanner m a => DupScanner (ReaderT Env m) a where
  scan = lift . scan
  mapify = lift . mapify
  display = lift . display

entry :: (MonadReader Env m, DupScanner m a) => m (DupMap a)
entry = ask >>= (scan >=> mapify)

showMap :: (Map a [Text]) -> Text
showMap mp = M.foldrWithKey f T.empty mp
  where f _ names acc = case names of
          [] -> acc
          xs -> showNames xs acc

showNames :: [Text] -> Text -> Text
showNames names acc = (T.intercalate ", " names) `T.append` "\n\n" `T.append` acc