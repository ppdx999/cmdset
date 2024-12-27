module Main where

import Test.Hspec
import Lib (decodeUrlEncodedText, query2kvs, kvs2text)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

test_decodeUrlEncodedText :: String -> String -> Expectation
test_decodeUrlEncodedText input expected =
    decodeUrlEncodedText (T.pack input) `shouldBe` T.pack expected


test_query2kvs :: String -> [(String, String)] -> Expectation
test_query2kvs input expected =
    query2kvs (T.pack input)
    `shouldBe`
    Map.fromList (map (bimap T.pack T.pack) expected)

test_kvs2text :: [(String, String)] -> String -> Expectation
test_kvs2text input expected =
    kvs2text (Map.fromList (map (bimap T.pack T.pack) input))
    `shouldBe`
    T.pack expected

main :: IO ()
main = hspec $ do
    describe "decodeUrlEncodedText" $ do
      it "decodes basic URL-encoded strings" $ do
        test_decodeUrlEncodedText "%20" " "
        test_decodeUrlEncodedText "+" " "
        test_decodeUrlEncodedText "%41" "A"
        test_decodeUrlEncodedText "%7E" "~"

      it "decodes multibyte characters" $ do
        test_decodeUrlEncodedText "%E3%81%82%E3%81%84%E3%81%86" "あいう"
        test_decodeUrlEncodedText "%D0%9F%D1%80%D0%B8%D0%B2%D0%B5%D1%82" "Привет"

      it "handles invalid URL-encoded sequences" $ do
        test_decodeUrlEncodedText "%G1" "%G1"
        test_decodeUrlEncodedText "%" "%"
        test_decodeUrlEncodedText "%41%" "A%"

      it "handles edge cases" $ do
        test_decodeUrlEncodedText "" ""
        test_decodeUrlEncodedText "%%%" "%%%"
        test_decodeUrlEncodedText "Hello+World" "Hello World"
        test_decodeUrlEncodedText "Hello%20World" "Hello World"

      it "decodes special characters" $ do
        test_decodeUrlEncodedText "%21" "!"
        test_decodeUrlEncodedText "%2F" "/"
        test_decodeUrlEncodedText "%3F" "?"
        test_decodeUrlEncodedText "%3D" "="
        test_decodeUrlEncodedText "%0A" "\n"

      it "handles combined cases" $ do
        test_decodeUrlEncodedText "%E3%81%82+%3F+invalid%G1" "あ ? invalid%G1"

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

      it "e2e" $ do
        test_query2kvs
          "place=%E6%9D%B1%E4%BA%AC%0D%0A%E5%A4%A7%E9%98%AA&country=ja+pan"
          [("place", "東京\n大阪"), ("country", "ja pan")]

    describe "kvs2text" $ do
      it "converts a key-value store into a text" $ do
        test_kvs2text [] ""
        test_kvs2text [("a", "b")] "a b\n"
        test_kvs2text [("a", "b"), ("c", "d")] "a b\nc d\n"

      it "escape lf" $ do
        test_kvs2text [("a", "b\nc")] "a b\\nc\n"
        test_kvs2text [("a", "b"), ("c", "d\ne")] "a b\nc d\\ne\n"

      it "e2e" $ do
        test_kvs2text
          [("place", "東京\n大阪"), ("country", "ja pan")]
          "country ja pan\nplace 東京\\n大阪\n"
