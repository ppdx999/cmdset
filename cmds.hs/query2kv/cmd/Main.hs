-- module Main where

import Shared (showMsgs)
import Lib (query2kvs, kvs2text)
import System.Environment (getArgs, getEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


usage :: IO ()
usage = showMsgs
  [ "Usage: query2kv"
  , "Version: 0.1.0"
  , "Description:"
  , "  query2kv reads the QUERY_STRING environment variable"
  , " and prints the key-value pairs to stdout."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> getEnv "QUERY_STRING" >>= TIO.putStr . kvs2text . query2kvs . T.pack
    _     -> usage
