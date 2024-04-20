module Main where

import Test.Hspec
import P3.Example1
import P3.Example2

main :: IO ()
main = hspec $ do
    specEx1
    specEx2