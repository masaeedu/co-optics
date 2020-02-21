module Monoidal.Applicative where

import MyPrelude

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Bifunctor

class Functor f => Apply f
  where
  zip :: f a -> f b -> f (a, b)

class Apply f => Applicative f
  where
  pure:: a -> f a

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = uncurry f <$> fa `zip` fb

instance Apply Maybe
  where
  Nothing `zip` _       = Nothing
  _       `zip` Nothing = Nothing
  Just x  `zip` Just y  = Just (x, y)

instance Applicative Maybe
  where
  pure = Just

instance Apply []
  where
  [] `zip` _ = []
  (x : xs) `zip` ys = ((x, ) <$> ys) ++ zip xs ys

instance Applicative []
  where
  pure = (:[])

instance Monad m => Apply (StateT s m)
  where
  StateT x `zip` StateT y = StateT $ \s -> x s >>= \(a, s') -> fmap (first (a,)) $ y s'

instance Monad m => Applicative (StateT s m)
  where
  pure a = StateT $ \s -> return $ (a, s)

instance (Apply m, Semigroup w) => Apply (WriterT w m)
  where
  WriterT x `zip` WriterT y = WriterT $ liftA2 (\(a, w1) (b, w2) -> ((a, b), w1 <> w2)) x y

instance (Applicative m, Monoid w) => Applicative (WriterT w m)
  where
  pure a = WriterT $ fmap (, mempty) $ pure a

instance Apply IO where
  fa `zip` fb = fa >>= \a -> fb >>= \b -> return (a, b)

instance Applicative IO where
  pure = return
