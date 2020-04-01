{-# LANGUAGE RebindableSyntax, LambdaCase #-}
module Main where

import MyPrelude

import Examples.Misc
import Examples.Biparsing
import Examples.Bigeneration

main :: IO ()
main = do
  testBigeneration
  testBiparsing
  miscCoprismStuff
