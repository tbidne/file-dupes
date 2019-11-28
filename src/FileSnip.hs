module FileSnip (FileSnip(..)) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

newtype FileSnip = FileSnip { unSnip :: (Text, ByteString) }
  deriving Show