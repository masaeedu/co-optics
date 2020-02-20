module Main where

import Data.Profunctor (Profunctor(..))
import Control.Arrow (arr)

import Optics
import Profunctor.Re ()
import Profunctor.Joker ()
import Profunctor.Kleisli (Kleisli(..))

real2int :: Prism' Double Int
real2int = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

int2real :: Coprism' Int Double
int2real = re real2int

double :: Iso' Double Double
double = dimap (/ 2) (* 2)

halve :: Iso' Double Double
halve = re double

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with integers as if they were doubles
  print $ speculate @Maybe int2real (pure . (/ 2)) $ 2 -- Just 1
  print $ speculate @Maybe int2real (pure . (/ 2)) $ 3 -- Nothing

  let input = [42, 4.2, 5, 1.5]

  -- Another, slightly stupider notion of "arrow" is one with no input
  -- This basically corresponds to filtering a container
  print $ int2real           `whittle` input -- [42, 5]

  -- We can make up more sophisticated coprisms for deeper filtering
  print $ (int2real . halve) `whittle` input -- [21]

  print $ re (liftPrism real2int) `whittle` (("foo",) <$> input) -- [("foo", 42), ("foo", 5)]
