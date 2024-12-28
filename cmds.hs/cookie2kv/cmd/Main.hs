module Main where

import Shared (showMsgs, showKVS)
import Lib (cookie2kvs)
import System.Environment (getArgs, getEnv)
import qualified Data.Text as T

usage :: IO ()
usage = showMsgs
  [ "Usage: cookie2kv"
  , "Version: 0.1.0"
  , "Description:"
  , "  cookie2kv reads HTTP_COOKIE from the environment"
  , " and prints the key-value pairs to stdout."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> getEnv "HTTP_COOKIE"
             >>= showKVS
               . cookie2kvs
               . T.pack

    _     -> usage
