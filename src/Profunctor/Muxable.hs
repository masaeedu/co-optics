{-# LANGUAGE LambdaCase #-}
module Profunctor.Muxable where

import MyPrelude
import Data.Profunctor

import Profunctor.Mux

import Optics

class Functor f => Muxable f
  where
  bundle :: (Choice p, Visitor p) => f (p a b) -> p (f a) (f b)

instance Muxable []
  where
  bundle [] = rmap (const []) start
  bundle (x : xs) = uncons $ _Just $ x /\ bundle xs
