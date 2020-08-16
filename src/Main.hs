{-# LANGUAGE RebindableSyntax, LambdaCase #-}
module Main where

import Examples.Misc
import Examples.Biparsing
import Examples.Bigeneration

main :: IO ()
main = do
  testBigeneration
  testBiparsing
  miscCoprismStuff
