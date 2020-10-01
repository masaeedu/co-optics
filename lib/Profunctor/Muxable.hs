{-# LANGUAGE LambdaCase #-}
module Profunctor.Muxable where

import Data.Profunctor

import Profunctor.Mux
import Profunctor.Lazy

import Optics

class Functor f => Muxable f
  where
  bundle :: (Choice p, Visitor p, Lazy2 p) => f (p a b) -> p (f a) (f b)

instance Muxable []
  where
  bundle [] = rmap (const []) start
  bundle (x : xs) = uncons $ _Just $ (x /\) $ defer $ \_ -> bundle xs
