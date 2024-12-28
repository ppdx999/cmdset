-- module Main where

import Shared (showMsgs, showKVS)
import Lib (form2kvs)
import System.Environment (getArgs, getEnv)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

usage :: IO ()
usage = showMsgs
  [ "Usage: form2kv"
  , "Version: 0.1.0"
  , "Description:"
  , "  form2kv reads Content-Length bytes from stdin"
  , " and prints the key-value pairs to stdout."
  ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> mainProc
    _     -> usage


mainProc :: IO ()
mainProc = do
      -- Get CONTENT_LENGTH environment variable
    contentLength <- read <$> getEnv "CONTENT_LENGTH" :: IO Int

    -- Read the input from stdin
    bytes <- B.getContents
    let input = TE.decodeUtf8 $ B.take contentLength bytes

    -- Parse the input
    let kvs = form2kvs input

    -- Print the output
    showKVS kvs
