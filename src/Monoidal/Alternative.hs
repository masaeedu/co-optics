module Monoidal.Alternative where

import MyPrelude

import qualified Control.Applicative as A

import Data.Bifunctor

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy (WriterT(..))

import Hedgehog

import Monoidal.BaseFunctor

class Functor f => Alt f
  where
  (<|>) :: f a -> f b -> f (a + b)

class Alt f => Alternative f
  where
  empty :: f a

instance {-# OVERLAPPABLE #-} (A.Applicative f, Alternative f) => A.Alternative f
  where
  empty = empty
  f <|> g = fmap (either id id) $ f <|> g

instance A.Alternative f => Alt (BaseFunctor f)
  where
  f <|> g = (Left <$> f) A.<|> (Right <$> g)

instance A.Alternative f => Alternative (BaseFunctor f)
  where
  empty = A.empty

deriving via (BaseFunctor Maybe) instance Alt Maybe
deriving via (BaseFunctor Maybe) instance Alternative Maybe

deriving via (BaseFunctor []) instance Alt []
deriving via (BaseFunctor []) instance Alternative []

instance Alt f => Alt (StateT s f)
  where
  StateT fa <|> StateT fb = StateT $ \s -> fmap (either (first Left) (first Right)) $ fa s <|> fb s

instance Alternative f => Alternative (StateT s f)
  where
  empty = StateT $ const empty

instance Alt f => Alt (WriterT s f)
  where
  WriterT fa <|> WriterT fb = WriterT $ fmap (either (first Left) (first Right)) $ fa <|> fb

instance Alternative f => Alternative (WriterT s f)
  where
  empty = WriterT $ empty

deriving via (BaseFunctor Gen) instance Alt Gen
deriving via (BaseFunctor Gen) instance Alternative Gen

instance Semigroup m => Alt (Either m)
  where
  Left x  <|> Left  y = Left $ x <> y
  Left _  <|> Right x = Right $ Right x
  Right x <|> _       = Right $ Left x

instance Monoid m => Alternative (Either m)
  where
  empty = Left mempty
