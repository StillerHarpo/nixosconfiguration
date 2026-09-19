module Main where

import Bookmarks

main :: IO ()
main = isBookmark (\bookmarks -> normal bookmarks <> work bookmarks)
