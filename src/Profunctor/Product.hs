module Profunctor.Product where

import MyPrelude
import Prelude (Applicative(..))

import Data.Bifunctor.Product

pfst :: Product p q a b -> p a b
pfst (Pair x _) = x

psnd :: Product p q a b -> q a b
psnd (Pair _ x) = x

instance (Functor (p a), Functor (q a)) => Functor (Product p q a)
  where
  fmap f (Pair p q) = Pair (fmap f p) (fmap f q)

instance (Applicative (p a), Applicative (q a)) => Applicative (Product p q a)
  where
  Pair a b <*> Pair c d = Pair (a <*> c) (b <*> d)
  pure a = Pair (pure a) (pure a)

instance (Monad (p a), Monad (q a)) => Monad (Product p q a)
  where
  return = pure
  Pair a b >>= f = Pair (a >>= pfst . f) (b >>= psnd . f)
