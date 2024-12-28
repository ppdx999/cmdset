module Main where

import Shared (showMsgs, showKVS)
import Lib (query2kvs)
import System.Environment (getArgs, getEnv)
import qualified Data.Text as T


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
    []    -> getEnv "QUERY_STRING" >>= showKVS . query2kvs . T.pack
    _     -> usage
