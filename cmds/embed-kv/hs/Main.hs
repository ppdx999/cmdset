module Main where

import Lib (parseKV, embed)
import System.Environment (getArgs)
import System.IO (stderr, hPutStr)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

usage :: IO ()
usage = do
    hPutStr stderr "Usage: embed-kv <template> <data>\n"
    hPutStr stderr "Version: 0.1.0\n"

readF :: String -> IO T.Text
readF "-" = TIO.getContents
readF f = TIO.readFile f

main :: IO ()
main = do
  args <- getArgs
  case args of
    [t,d] -> TIO.putStr =<< embed <$> readF t <*> (parseKV <$> readF d)
    _     -> usage
