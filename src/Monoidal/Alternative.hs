module Monoidal.Alternative where

import MyPrelude

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy (WriterT(..))
import Data.Bifunctor

class Functor f => Alt f
  where
  (<|>) :: f a -> f b -> f (Either a b)

class Alt f => Alternative f
  where
  empty :: f a

instance Alt Maybe
  where
  Nothing <|> a = Right <$> a
  a       <|> _ = Left  <$> a

instance Alternative Maybe
  where
  empty = Nothing

instance Alt []
  where
  xs <|> ys = (Left <$> xs) ++ (Right <$> ys)

instance Alternative []
  where
  empty = []

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
