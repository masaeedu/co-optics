module Profunctor.Mux where

import MyPrelude

import Data.Profunctor
import Data.Bifunctor.Product

import Profunctor.Joker
import Profunctor.Kleisli
import Monoidal.Applicative

class Profunctor p => Mux p
  where
  (/\) :: p a b -> p c d -> p (a, c) (b, d)

class Mux p => Visitor p
  where
  start :: p a ()

instance Apply f => Mux (Kleisli f)
  where
  Kleisli f /\ Kleisli g = Kleisli $ \(a, c) -> liftA2 (,) (f a) (g c)

instance Applicative f => Visitor (Kleisli f)
  where
  start = Kleisli $ const $ pure ()

instance Apply f => Mux (Joker f)
  where
  Joker x /\ Joker y = Joker $ liftA2 (,) x y

instance Applicative f => Visitor (Joker f)
  where
  start = Joker $ pure ()

instance (Mux p, Mux q) => Mux (Product p q)
  where
  Pair a b /\ Pair c d = Pair (a /\ c) (b /\ d)

instance (Visitor p, Visitor q) => Visitor (Product p q)
  where
  start = Pair start start
