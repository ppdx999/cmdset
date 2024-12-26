module Main (main) where


import Test.Hspec
import Lib (escapeHTML)
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "escapeHTML" $ do
        it "escapes HTML special characters" $ do
            escapeHTML
              (T.pack "<>&'\"")
              `shouldBe`
              T.pack "&lt;&gt;&amp;&#39;&quot;"

        it "escapes HTML special characters in a text" $ do
            escapeHTML
              (T.pack "<script>alert('XSS')</script>")
              `shouldBe`
              T.pack "&lt;script&gt;alert(&#39;XSS&#39;)&lt;/script&gt;"

        it "empty text" $ do
            escapeHTML
              (T.pack "")
              `shouldBe`
              T.pack ""

        it "text without special characters" $ do
            escapeHTML
              (T.pack "Hello, world!")
              `shouldBe`
              T.pack "Hello, world!"
