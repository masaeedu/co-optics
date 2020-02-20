{-# LANGUAGE TupleSections #-}
module Main where

import Data.Profunctor (Profunctor(..))
import Control.Arrow (Kleisli(..))
import Control.Arrow (arr)
import Data.Bifunctor.Joker (Joker(..))

import Optics
import Profunctor.Re ()
import Profunctor.Joker ()
import Profunctor.Kleisli ()

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
  print $ runKleisli @Maybe (int2real $ arr (/ 2)) $ 2 -- Just 1
  print $ runKleisli @Maybe (int2real $ arr (/ 2)) $ 3 -- Nothing

  let input = [42, 4.2, 5, 1.5]

  -- Another, slightly stupider notion of "arrow" is one with no input
  -- This basically corresponds to filtering a container
  print $ runJoker $ int2real $ Joker $ input -- [42, 5]

  -- We can make up more sophisticated coprisms for deeper filtering
  print $ runJoker $ int2real . halve $ Joker $ input -- [21]

  print $ runJoker $ re (liftPrism real2int) $ Joker $ fmap ("foo",) $ input -- [("foo", 42), ("foo", 5)]
