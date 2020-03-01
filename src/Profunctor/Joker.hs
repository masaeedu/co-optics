module Profunctor.Joker where

import MyPrelude

import Data.Profunctor (Cochoice(..), Costrong(..))
import Data.Bifunctor.Joker (Joker(..))

import Monoidal.Alternative (Alt(..))
import Monoidal.Filterable
import Monoidal.Applicative

instance Filterable m => Cochoice (Joker m)
  where
  unleft (Joker m) = Joker $ fst $ partition m

instance Functor m => Costrong (Joker m)
  where
  unfirst (Joker m) = Joker $ fst <$> m

instance Alt m => Alt (Joker m a)
  where
  Joker f <|> Joker g = Joker $ f <|> g

instance Apply m => Apply (Joker m a)
  where
  Joker f `zip` Joker a = Joker $ f `zip` a

instance Applicative m => Applicative (Joker m a)
  where
  pure a = Joker $ pure a

instance (Applicative m, Monad m) => Monad (Joker m a)
  where
  return = pure
  Joker f >>= amb = Joker $ f >>= runJoker . amb
