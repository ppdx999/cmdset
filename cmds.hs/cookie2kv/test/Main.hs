module Main where

import Test.Hspec
import Lib (add)

main :: IO ()
main = hspec $ do
    describe "add" $ do
        it "adds two numbers" $ do
            add 1 2 `shouldBe` 3
