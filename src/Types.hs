module Types
( FileSnip(..)
, DupMap
, showMap
) where

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
          | L.length names > 1 = showNames names acc
          | otherwise          = acc

showNames :: [Text] -> Text -> Text
showNames names acc = (T.intercalate ", " names) `T.append` "\n\n" `T.append` acc

newtype FileSnip a = FileSnip { unSnip :: (Text, ByteString) }
  deriving Show