module Main where

import Test.Hspec
import Lib (parseKV, embed)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "parseKV" $ do
        it "parses a key-value formatted string" $ do
            parseKV $ T.pack "key1 value"
            `shouldBe`
            Map.fromList [(T.pack "key1", T.pack "value")]

        it "parses a key-value formatted string with multiple lines" $ do
            parseKV $ T.pack "key1 value\nkey2 value 2"
            `shouldBe`
            Map.fromList [(T.pack "key1", T.pack "value"), (T.pack "key2", T.pack "value 2")]

        it "parses a key-value formatted string with leading and trailing spaces" $ do
            parseKV $ T.pack " key1 value "
            `shouldBe`
            Map.fromList [(T.pack "key1", T.pack "value")]

        it "parses empty string" $ do
            parseKV $ T.pack ""
            `shouldBe`
            Map.empty

    describe "embed" $ do
        it "embeds key-value pairs into a template" $ do
            embed (T.pack "Hello, name!") (Map.fromList [(T.pack "name", T.pack "world")])
            `shouldBe`
            T.pack "Hello, world!"

        it "embeds key-value pairs into a template with multiple keys" $ do
            embed (T.pack "Hello, name! You are age years old.") (Map.fromList [(T.pack "name", T.pack "world"), (T.pack "age", T.pack "100")])
            `shouldBe`
            T.pack "Hello, world! You are 100 years old."

        it "embeds key-value pairs into a template with missing keys" $ do
            embed 
              (T.pack "Hello, name! You are age years old.")
              (Map.fromList [(T.pack "name", T.pack "world")])
            `shouldBe`
            T.pack "Hello, world! You are age years old."

        it "embeds key-value pairs into a template with empty keys" $ do
            embed
              (T.pack "Hello, name! You are age years old.")
              (Map.fromList [(T.pack "name", T.pack ""), (T.pack "age", T.pack "100")])
            `shouldBe`
            T.pack "Hello, ! You are 100 years old."

        it "embeds key-value pairs into a template with empty template" $ do
            embed (T.pack "") (Map.fromList [(T.pack "name", T.pack "world")])
            `shouldBe`
            T.pack ""

        it "embeds key-value pairs into a template with empty key-value pairs" $ do
            embed (T.pack "Hello, name! You are age years old.") Map.empty
            `shouldBe`
            T.pack "Hello, name! You are age years old."

        it "embeds key-value pairs into a template with empty key-value pairs and empty template" $ do
            embed (T.pack "") Map.empty
            `shouldBe`
            T.pack ""
