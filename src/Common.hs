module Common
( defaultSearch
, searchPath
, searchFull
) where

import System.FilePath.Find

import Types

onlyFiles :: FilterPredicate
onlyFiles = fileType ==? RegularFile

defaultSearch :: IO [FilePath]
defaultSearch = find always always "./"

searchPath :: FilePath -> IO [FilePath]
searchPath = find always onlyFiles

searchFull :: RecursionPredicate -> FilterPredicate -> FilePath -> IO [FilePath]
searchFull = find