module Profunctor.Joker where

import MyPrelude

import qualified Prelude as P (Applicative (..), (<*>))

import Data.Profunctor (Cochoice(..), Costrong(..))
import Data.Bifunctor.Joker (Joker(..))

import Monoidal.Alternative (Alt(..))
import Monoidal.Filterable

instance (Filterable m) => Cochoice (Joker m)
  where
  unleft (Joker m) = Joker $ fst $ partition m

instance Functor m => Costrong (Joker m)
  where
  unfirst (Joker m) = Joker $ fst <$> m

instance Alt m => Alt (Joker m a)
  where
  Joker f <|> Joker g = Joker $ f <|> g

instance P.Applicative m => P.Applicative (Joker m a)
  where
  Joker f <*> Joker a = Joker $ f P.<*> a
  pure a = Joker $ P.pure a

instance Monad m => Monad (Joker m a)
  where
  return = P.pure
  Joker f >>= amb = Joker $ f >>= runJoker . amb
