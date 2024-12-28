module Lib (cookie2kvs) where

import Shared (decodeUrlEncodedText, KVS)
import Data.Bifunctor (bimap, second)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

cookie2kvs :: T.Text -> KVS
cookie2kvs txt
  | txt == T.pack "" = Map.empty
  | otherwise        = Map.fromList . map (decodePair . toPair) . toLines $ txt
  where
    toLines :: T.Text -> [T.Text]
    toLines = T.splitOn $ T.pack ";"

    toPair :: T.Text -> (T.Text, T.Text)
    toPair = second (T.drop 1) . T.breakOn (T.pack "=")

    decodePair :: (T.Text, T.Text) -> (T.Text, T.Text)
    decodePair = bimap decodeUrlEncodedText decodeUrlEncodedText
