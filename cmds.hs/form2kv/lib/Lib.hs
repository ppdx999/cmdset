module Lib
( decodeUrlEncodedText
, form2kvs
, kvs2text
) where

import Network.URI.Encode (decodeText)
import Data.Bifunctor (bimap, second)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

type KVS = Map.Map T.Text T.Text

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

form2kvs :: T.Text -> KVS
form2kvs txt
  | txt == T.pack "" = Map.empty
  | otherwise        = Map.fromList . map (decodePair . line2pair) . form2lines $ txt
  where
    form2lines :: T.Text -> [T.Text]
    form2lines = T.splitOn $ T.pack "&"

    line2pair :: T.Text -> (T.Text, T.Text)
    line2pair = second (T.drop 1) . T.breakOn (T.pack "=")

    decodePair :: (T.Text, T.Text) -> (T.Text, T.Text)
    decodePair = bimap decodeUrlEncodedText decodeUrlEncodedText


kvs2text :: KVS -> T.Text
kvs2text = Map.foldrWithKey toText (T.pack "")
  where
    toText :: T.Text -> T.Text -> T.Text -> T.Text
    toText k v acc
      | v == T.pack "" = T.concat [escapeLF k, T.pack "\n", acc]
      | otherwise      = T.concat [escapeLF k, T.pack " ", escapeLF v, T.pack "\n", acc]

    escapeLF :: T.Text -> T.Text
    escapeLF = T.replace (T.pack "\n") (T.pack "\\n")
