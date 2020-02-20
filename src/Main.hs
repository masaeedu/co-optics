module Main where

import Control.Arrow (Kleisli(..))
import Control.Arrow (arr)
import Data.Bifunctor.Joker (Joker(..))

import Optics
import Profunctor.Re ()
import Profunctor.Joker ()
import Profunctor.Kleisli ()

_int :: Prism' Double Int
_int = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with integers as if they were doubles
  print $ runKleisli @Maybe (re _int $ arr (/ 2)) $ 2 -- Just 1
  print $ runKleisli @Maybe (re _int $ arr (/ 2)) $ 3 -- Nothing

  -- Another, slightly stupider notion of "arrow" is one with no input
  print $ runJoker $ re _int $ Joker $ [42, 4.2, 5, 1.5]  -- [42, 5]
