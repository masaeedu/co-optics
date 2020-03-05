{-# LANGUAGE LambdaCase #-}
module Examples.Biparsing where

import MyPrelude

import GHC.Natural (Natural, intToNatural, naturalToInt)

import Data.Profunctor (Profunctor(..))
import Data.Foldable (traverse_)

import Data.Digit (DecDigit(..))

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Muxable

import Monoidal.Applicative

import Optics

import Examples.Biparsing.Common
import Examples.Biparsing.JSON

testBiparsing :: IO ()
testBiparsing = do
  print $ biparse digit $ ""
  print $ biparse digit $ "c5"
  print $ biparse digit $ "5c"

  print $ biprint digit $ DecDigit2

  print $ biparse (many digit) $ "12..."
  print $ biprint (many digit) $ [DecDigit4, DecDigit2]

  let
    digitOrChar :: BP' (DecDigit + Char)
    digitOrChar = digit \/ anychar

  print $ biparse digitOrChar $ ""
  print $ biparse digitOrChar $ "5"
  print $ biparse digitOrChar $ "c2"

  print $ biparse (many digitOrChar) $ "a1b"

  print $ biparse nat $ "123131234"
  print $ biparse nat $ "123a123"

  let intOrChar = nat \/ anychar
  print $ biparse intOrChar $ "123a123"
  print $ biparse (many intOrChar) $ "123a123"
  print $ biprint (many intOrChar) $ [Left 123, Right 'a', Left 123]

  let
    delimitedInts :: BP' [(Natural, Char)]
    delimitedInts = many $ nat /\ anychar

  print $ biparse delimitedInts $ "12,13,14."
  print $ biprint delimitedInts $ [(12,','),(13,','),(14,'.')]

  let
    char2space :: Prism' Char Char
    char2space = prism id (\case { ' ' -> Right ' '; c -> Left c })

    space :: BP' Char
    space = re char2space anychar

    spacesThenChar :: BP' (String, Char)
    spacesThenChar = many space /\ anychar

  print $ biparse spacesThenChar $ "  f"

  let
    spacesThenChar' :: BP (String, Char) Char
    spacesThenChar' = rmap snd spacesThenChar

  print $ biparse spacesThenChar' $ "  f"

  print $ biprint spacesThenChar' $ ("    ", 'f')

  let
    spacesThenChar'' :: BP' Char
    spacesThenChar'' = iso ("", ) snd spacesThenChar

  print $ biparse spacesThenChar'' $ " f"
  print $ biparse spacesThenChar'' $ "   f"
  print $ biprint spacesThenChar'' $ 'f'

  let
    lstring :: BP' String
    lstring = do
      n  <- nat     & lmap (intToNatural . length)
      _  <- space   & lmap (const ' ')
      cs <- anychar & replicate (naturalToInt n) & bundle
      pure cs

  print $ biparse lstring $ "10 whoops"
  print $ biparse lstring $ "6 lambda calculus"
  print $ biprint lstring $ "SKI"

  let
    manyints :: BP' [(Int, Char)]
    manyints = many (int /\ char ',')

  print $ biparse manyints $ "-10,11,0,--11,foo"
  print $ biprint manyints $ [(-10, ','), (11, ','), (0, ',')]

  print $ biparse jsonString $ "\"helloworld\""
  print $ biprint jsonString $ "helloworld"

  print $ biprint jsonString $ "\tfoo\bbar\""

  print $ biparse jsonValue $ "[1, [{ \"foo\": 11 }, 5]]"
  traverse_ putStrLn $ biprint_ jsonValue =<< biparse_ jsonValue "[1.3E11, [{ \"foo\": [\"hello,\\nworld\", false] }, 5.5]]"

  print $ biparse jsonValue $ "[]"
