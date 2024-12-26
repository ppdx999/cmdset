module Main where

import Shared (showMsgs, readFT)
import Lib (escapeHTML)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

usage :: IO ()
usage = showMsgs
    [
    "Usage: escape-html",
    "Description:",
    "  Escape HTML special characters in the input text.",
    "  read from stdin and write to stdout.",
    "Version: 0.1.0"
    ]


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> mainProc "-"
        [f] -> mainProc f
        _  -> usage

mainProc :: String  -> IO ()
mainProc f = readFT f >>= TIO.putStr . escapeHTML
