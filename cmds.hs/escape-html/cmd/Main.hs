module Main where

import Lib (escapeHTML)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

usage :: IO ()
usage = do
    hPutStr stderr "Usage: escape-html\n"
    hPutStr stderr "Description:\n"
    hPutStr stderr "  Escape HTML special characters in the input text.\n"
    hPutStr stderr "  read from stdin and write to stdout.\n"
    hPutStr stderr "Version: 0.1.0"


readF :: String -> IO T.Text
readF "-" = TIO.getContents
readF f   = TIO.readFile f

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> TIO.interact escapeHTML
        [f] -> readF f >>= TIO.putStr . escapeHTML
        _  -> usage
