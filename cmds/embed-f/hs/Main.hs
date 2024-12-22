module Main where

import System.Environment (getArgs)
import System.IO as IO
import Data.ByteString.Char8 as BS

usage :: IO ()
usage = do
    IO.hPutStr stderr "Usage: embed-f <keyword> <data> <template>\n"
    IO.hPutStr stderr "Version: 0.1.0"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [k, d, t] -> do
            d' <- BS.dropWhileEnd (== '\n') <$> BS.readFile d
            t' <- BS.readFile t
            BS.putStr $ embed k d' t'
        _  -> usage

embed :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString
embed k d t = BS.unlines $ Prelude.map (procLine k d) (BS.lines t)

procLine :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString
procLine k d l = if BS.isInfixOf (BS.pack k) l then d else l
