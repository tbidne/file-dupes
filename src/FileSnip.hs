module FileSnip where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

newtype FileSnip a = FileSnip { unSnip :: (Text, ByteString) }
  deriving Show