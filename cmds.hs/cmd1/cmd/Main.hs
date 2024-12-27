module Main where

import Lib (add)

main :: IO ()
main = putStrLn $ show $ add 1 2
