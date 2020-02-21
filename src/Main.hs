{-# LANGUAGE RebindableSyntax #-}
module Main where

import MyPrelude

import Data.Profunctor (Profunctor(..))
import Data.Map.Strict (Map, fromList)
import Control.Monad.State.Lazy

import Optics

import Profunctor.Re ()
import Profunctor.Joker ()
import Profunctor.Branching

import Monoidal.Filterable ()
import Monoidal.Applicative

import Parser

r2i :: Prism' Double Int
r2i = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

i2r :: Coprism' Int Double
i2r = re r2i

double :: Fractional n => Iso' n n
double = dimap (/ 2) (* 2)

halve :: Fractional n => Iso' n n
halve = re double

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with integers as if they were doubles
  print $ speculate i2r (Just . (/ 2)) $ 2 -- Just 1
  print $ speculate i2r (Just . (/ 2)) $ 3 -- Nothing

  -- Nonsense example where we find the factors of a number by just performing floating
  -- point division with prime factors
  let
    quotients :: Double -> Map Int Double
    quotients x = fromList [(1, x / 1), (2, x / 2), (3, x / 3), (5, x / 5)]

  -- Because a @Map@ is filterable, we can focus onto the integer results, discarding
  -- anything that didn't divide cleanly
  print $ speculate i2r quotients $ 15 -- fromList [(1, 15), (3, 5), (5, 3)]

  -- Another, slightly stupider notion of "arrow" is one with no input
  -- This basically corresponds to filtering a container
  let input = [42, 4.2, 5, 1.5]
  print $ i2r `whittle` input -- [42, 5]

  -- Coprisms interact appropriately with other optics
  print $ (i2r . halve) `whittle` input -- [21]

  let input' = ("foo",) <$> input
  print $ re (liftPrism r2i) `whittle` input' -- [("foo", 42), ("foo", 5)]

  -- There's some weird filterables out there
  -- E.g. parsers are filterable
  print $ flip (runStateT @String @Maybe) "0.5"
        $ re r2i `whittle`
          do
            s <- get
            pure $ read s -- Nothing

  -- Try parsing as a digit, if it fails, try parsing as a character
  let dorc = digit \/ char
  print $ biparse dorc $ ""   -- Nothing
  print $ biparse dorc $ "5"  -- Just (Left DecDigit5, "")
  print $ biparse dorc $ "c2" -- Just (Right 'c', "")

  print $ biparse (each dorc) $ "a1b2c3d4" -- Just ([Right 'a',Left DecDigit1,Right 'b',Left DecDigit2,Right 'c',Left DecDigit3,Right 'd',Left DecDigit4],"")

  print $ biparse int $ "123131234"                -- Just (123131234,"")
  print $ biparse int $ "123a123"                  -- Just (123, "a123")
  print $ biparse (int \/ char) $ "123a123"        -- Just (Left 123,"a123")
  print $ biparse (each $ int \/ char) $ "123a123" -- Just ([Left 123,Right 'a',Left 123],"")

