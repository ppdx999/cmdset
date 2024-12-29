module Main where

import Shared (showMsgs, readFT, parseKVS, KVS)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Map as Map

usage :: IO ()
usage = showMsgs
  [
    "Usage: embed-kv <template> <data>",
    "Version: 0.1.0"
  ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    [t]   -> mainProc t "-"
    [t,d] -> mainProc t d
    _     -> usage

mainProc :: String -> String -> IO ()
mainProc t d = TIO.putStr =<< embed <$> readFT t <*> (parseKVS <$> readFT d)

embed :: T.Text -> KVS -> T.Text
embed = Map.foldrWithKey T.replace
