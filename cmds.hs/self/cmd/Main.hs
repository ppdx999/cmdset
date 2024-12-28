module Main where

import Shared (showMsgs)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B

usage :: IO ()
usage = showMsgs
  [ "Usage: self <f1> <f2> ... "
  , "Version: 0.1.0"
  , "Description: Extract fields from a file"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    xs -> mainProc xs

mainProc :: [String] -> IO ()
mainProc xs = do
  let fields = map read xs :: [Int]
  content <- B.getContents

  B.putStr
    $ B.unlines
    $ map (joinFields . filterFields fields . toFields)
    $ B.lines content

toFields :: B.ByteString -> [B.ByteString]
toFields line = filter (/= blank) $ B.split ' ' line
              where blank = B.pack ""

filterFields :: [Int] -> [B.ByteString] -> [B.ByteString]
filterFields nums fields = map (( fields !! ) . pred) nums

joinFields :: [B.ByteString] -> B.ByteString
joinFields = B.intercalate (B.pack " ")
