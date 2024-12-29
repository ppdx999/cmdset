module Main where

import Shared (showMsgs, parseKVS, KVS)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

usage :: IO ()
usage = showMsgs
  [ "Usage: readkv <key>"
  , "Version: 0.1.0"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [key]  -> mainProc $ T.pack key
    _      -> usage

mainProc :: T.Text -> IO ()
mainProc key = TIO.interact $ addLF . getItem key  . parseKVS

addLF :: T.Text -> T.Text
addLF t = if T.last t == '\n' then t else T.snoc t '\n'

getItem :: T.Text -> KVS -> T.Text
getItem key kvs = case Map.lookup key kvs of
  Just value -> value
  Nothing    -> T.pack ""
