module DupMap where

import Data.Map.Strict
import Data.Text

type DupMap a = Map a [Text]