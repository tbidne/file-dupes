{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module DupScanner
( DupScanner(..)
, entry) where

import Control.Monad.Reader

import Config
import FileSnip
import Hashing

class Monad m => DupScanner m where
  scan :: Env -> m [FileSnip]
  mapify :: [FileSnip] -> m DupMap
  display :: DupMap -> m ()

instance DupScanner m => DupScanner (ReaderT Env m) where
  scan = lift . scan
  mapify = lift . mapify
  display = lift . display

entry :: (MonadReader Env m, DupScanner m) => m DupMap
entry = ask >>= (scan >=> mapify)