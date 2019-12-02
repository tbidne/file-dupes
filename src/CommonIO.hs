module CommonIO
( scanIO
, searchPath
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import           System.FilePath.Find
import           System.IO

import Config
import FileSnip

onlyFiles :: FilterPredicate
onlyFiles = fileType ==? RegularFile

searchPath :: FilePath -> IO [FilePath]
searchPath = find always onlyFiles

scanIO :: T.Text -> Nat -> IO [FileSnip]
scanIO p sz = do
  files <- searchPath $ T.unpack p
  traverse (readSnip sz) files

readSnip :: Nat -> FilePath -> IO FileSnip
readSnip n p = withFile p ReadMode $ \h -> do
  sp <- B.hGetNonBlocking h (unNat n)
  return $ FileSnip (T.pack p, sp)