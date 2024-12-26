module Shared
( showMsgs
, readFT
) where

import System.IO (stderr, hPutStr)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

showMsgs :: [String] -> IO ()
showMsgs msgs = hPutStr stderr $ unlines msgs

readFT :: FilePath -> IO T.Text
readFT "-" = TIO.getContents
readFT f = TIO.readFile f
