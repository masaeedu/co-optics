module Profunctor.Product where

import MyPrelude

import Data.Bifunctor.Product

import Monoidal.Applicative

pfst :: Product p q a b -> p a b
pfst (Pair x _) = x

psnd :: Product p q a b -> q a b
psnd (Pair _ x) = x

instance (Functor (p a), Functor (q a)) => Functor (Product p q a)
  where
  fmap f (Pair p q) = Pair (fmap f p) (fmap f q)

instance (Apply (p a), Apply (q a)) => Apply (Product p q a)
  where
  Pair a b `zip` Pair c d = Pair (a `zip` c) (b `zip` d)

instance (Applicative (p a), Applicative (q a)) => Applicative (Product p q a)
  where
  pure a = Pair (pure a) (pure a)

instance (Applicative (p a), Monad (p a), Applicative (q a), Monad (q a)) => Monad (Product p q a)
  where
  return = pure
  Pair a b >>= f = Pair (a >>= pfst . f) (b >>= psnd . f)
