module Lib
( parseKV,
  embed
) where
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type KVS = Map.Map T.Text T.Text

parseKV :: T.Text -> KVS
parseKV input = Map.fromList $ map parseLine (T.lines input)
  where
    parseLine :: T.Text -> (T.Text, T.Text)
    parseLine line = trim $ splitLine $ T.strip line

    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\t'

    trim :: (T.Text, T.Text) -> (T.Text, T.Text)
    trim = bimap T.strip T.strip

    splitLine :: T.Text -> (T.Text, T.Text)
    splitLine = T.break isSpace

embed :: T.Text -> KVS -> T.Text
embed t kvs = T.unlines $ map (embedLine kvs) (T.lines t)
  where
    embedLine :: KVS -> T.Text -> T.Text
    embedLine kvs_ l = Map.foldrWithKey T.replace l kvs_
