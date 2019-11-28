# File Dups

[![Build Status](https://travis-ci.com/tbidne/file-dups.svg?branch=master)](https://travis-ci.com/tbidne/file-dups)

This program finds duplicate files based on hashing the contents.

## Building

`stack build`

## Executing

`stack exec  file-dupes-exe <path> <snip-size> <v|c|p>`

- `path` is the file system path to (recursively) search.

- `snip-size` is the first `N` bytes to hash for determing equality. Ultimately what size is necessary is dependent on the size of the files being considered. For example, I found using `3000000` (~3MB) necessary for files averaging 6MB.

- `v|c|p` represents the type of search to perform. Right now only `v` (default) is supported.

## Tests

`stack test`