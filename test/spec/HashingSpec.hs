module HashingSpec (spec) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           Test.Hspec
import           Test.Hspec.QuickCheck

import Arbitraries()
import FileSnip
import Hashing

spec :: Spec
spec = do
  describe "Hashing Tests" $ do
    prop "Verifying hash equality" vHash
    prop "Verifying map add" vMapAdd
    prop "Verifying map show" vMapShow

vHash :: FileSnip -> FileSnip -> Bool
vHash fs gs
  | f == g    = (hash fs :: SHA1) == (hash gs)
  | otherwise = True
  where f = snd $ unSnip fs
        g = snd $ unSnip gs

vMapAdd :: FileSnip -> DupMap -> Bool
vMapAdd fs mp =
  let mp' = addSnipToMap fs mp
  in memberSnip fs mp'

vMapShow :: DupMap -> Bool
vMapShow (SHA1Map mp) = M.foldrWithKey f True mp
  where out = showMap (SHA1Map mp)
        f :: SHA1 -> [T.Text] -> Bool -> Bool
        f _ vals b = b && cat vals `T.isInfixOf` out
          where cat :: [T.Text] -> T.Text
                cat = T.intercalate "\n"