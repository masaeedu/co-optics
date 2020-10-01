module Profunctor.Lazy where

import Data.Bifunctor.Product

import Profunctor.Kleisli
import Profunctor.Joker

class Lazy x
  where
  defer :: (() -> x) -> x

type Lazy2 p = (forall x y. Lazy (p x y))

instance Lazy (Joker m a b)
  where
  defer f = f ()

instance Lazy (Kleisli m a b)
  where
  defer f = Kleisli $ \a -> runKleisli (f ()) a

instance (Lazy (p a b), Lazy (q a b)) => Lazy (Product p q a b)
  where
  defer f = Pair (defer $ (\(Pair x _) -> x) . f) (defer $ (\(Pair _ x) -> x) . f)
