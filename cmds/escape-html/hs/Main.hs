module Main where

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

htmlEscape :: Char -> T.Text
htmlEscape '<'  = T.pack "&lt;"
htmlEscape '>'  = T.pack "&gt;"
htmlEscape '&'  = T.pack "&amp;"
htmlEscape '"'  = T.pack "&quot;"
htmlEscape '\'' = T.pack "&#39;"
htmlEscape c    = T.singleton c

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> TIO.interact (T.concatMap htmlEscape)
        _  -> usage
