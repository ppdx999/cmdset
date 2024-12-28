module Main where

import Test.Hspec
import Lib (query2kvs)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

test_query2kvs :: String -> [(String, String)] -> Expectation
test_query2kvs input expected =
    query2kvs (T.pack input)
    `shouldBe`
    Map.fromList (map (bimap T.pack T.pack) expected)

main :: IO ()
main = hspec $ do
    describe "query2kvs" $ do
      it "parses a form into a key-value store" $ do
        test_query2kvs "" []
        test_query2kvs "a=b" [("a", "b")]
        test_query2kvs "a=b&c=d" [("a", "b"), ("c", "d")]

      it "decodes URL-encoded keys and values" $ do
        test_query2kvs "%E3%81%82=%E3%81%84" [("あ", "い")]
        test_query2kvs "a%20b=c%20d&%21%3F%3D%3D=%3F%3D%3F" [("a b", "c d"), ("!?==", "?=?")]

      it "empty value" $ do
        test_query2kvs "a=" [("a", "")]
        test_query2kvs "a=&b=" [("a", ""), ("b", "")]

      it "escape lf" $ do
        test_query2kvs "a=b%0Ac" [("a", "b\nc")]

      it "extra space , tab , cr and lf are ignored" $ do
        test_query2kvs " a = b & c = d " [("a", "b"), ("c", "d")]
        test_query2kvs "a=b\t&c=d" [("a", "b"), ("c", "d")]
        test_query2kvs "a=b\r&c=d" [("a", "b"), ("c", "d")]
        test_query2kvs "a=b\n&c=d" [("a", "b"), ("c", "d")]

      it "e2e" $ do
        test_query2kvs
          "place=%E6%9D%B1%E4%BA%AC%0D%0A%E5%A4%A7%E9%98%AA&country=ja+pan"
          [("place", "東京\n大阪"), ("country", "ja pan")]
