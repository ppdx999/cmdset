module Shared
( showMsgs
, readFT
, KVS
, showKVS
, decodeUrlEncodedText
) where

import System.IO (stderr, hPutStr)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Network.URI.Encode (decodeText)

showMsgs :: [String] -> IO ()
showMsgs msgs = hPutStr stderr $ unlines msgs

type KVS = Map.Map T.Text T.Text

showKVS :: KVS -> IO ()
showKVS = TIO.putStr . Map.foldrWithKey toText (T.pack "")
  where
    toText :: T.Text -> T.Text -> T.Text -> T.Text
    toText k v acc
      | v == T.pack "" = T.concat [escapeLF k, T.pack "\n", acc]
      | otherwise      = T.concat [escapeLF k, T.pack " ", escapeLF v, T.pack "\n", acc]

    escapeLF :: T.Text -> T.Text
    escapeLF = T.replace (T.pack "\n") (T.pack "\\n")

decodeUrlEncodedText :: T.Text -> T.Text
decodeUrlEncodedText =
  T.replace (T.pack "\r") (T.pack "")
  . decodeText
  . T.replace (T.pack "+") (T.pack " ")
  . T.replace (T.pack "%20") (T.pack " ")
  . T.replace (T.pack "\n") (T.pack "")
  . T.replace (T.pack "\r") (T.pack "")
  . T.replace (T.pack "\t") (T.pack "")
  . T.replace (T.pack " ") (T.pack "")

readFT :: FilePath -> IO T.Text
readFT "-" = TIO.getContents
readFT f = TIO.readFile f
