module Main where

import Shared (showMsgs, readFT)
import Lib (parseKV, embed)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

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
mainProc t d = TIO.putStr =<< embed <$> readFT t <*> (parseKV <$> readFT d)
