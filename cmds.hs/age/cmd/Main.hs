module Main where

import Shared (showMsgs)
import System.Environment (getArgs)
import System.Directory (doesFileExist, getModificationTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

usage :: IO ()
usage = showMsgs
  [
    "Usage: age <file>",
    "Version: 0.1.0"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> mainProc f
    _   -> usage

mainProc :: String -> IO ()
mainProc f = do
  exists <- doesFileExist f
  if exists
    then do
      mt <- getModificationTime f
      ct <- getCurrentTime
      print ( floor (diffUTCTime ct mt) :: Int )
    else putStrLn "File does not exist"
