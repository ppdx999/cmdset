module Main (main) where

import Test.Hspec
import Shared (decodeUrlEncodedText)

test_decodeUrlEncodedText :: String -> String -> Expectation
test_decodeUrlEncodedText input expected =
    decodeUrlEncodedText (T.pack input) `shouldBe` T.pack expected

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
