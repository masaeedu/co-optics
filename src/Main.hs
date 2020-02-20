module Main where

import Data.Profunctor (Profunctor(..))
import Data.Map.Strict (Map, fromList)

import Optics
import Profunctor.Re ()
import Profunctor.Joker ()

r2i :: Prism' Double Int
r2i = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

i2r :: Coprism' Int Double
i2r = re r2i

double :: Iso' Double Double
double = dimap (/ 2) (* 2)

halve :: Iso' Double Double
halve = re double

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with integers as if they were doubles
  print $ speculate i2r (Just . (/ 2)) $ 2 -- Just 1
  print $ speculate i2r (Just . (/ 2)) $ 3 -- Nothing

  let
    quotients :: Double -> Map Int Double
    quotients x = fromList [(1, x / 1), (2, x / 2), (3, x / 3), (5, x / 5)]

  print $ speculate i2r quotients $ 15 -- fromList [(1, 15), (3, 5), (5, 3)]

  -- Another, slightly stupider notion of "arrow" is one with no input
  -- This basically corresponds to filtering a container
  let input = [42, 4.2, 5, 1.5]
  print $ i2r `whittle` input -- [42, 5]

  -- We can make up more sophisticated coprisms for deeper filtering
  print $ (i2r . halve) `whittle` input -- [21]

  let input' = ("foo",) <$> input

  print $ re (liftPrism r2i) `whittle` input' -- [("foo", 42), ("foo", 5)]
