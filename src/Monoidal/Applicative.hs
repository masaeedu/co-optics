module Monoidal.Applicative where

import MyPrelude

import qualified Control.Applicative as A

import Data.Bifunctor

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Test.QuickCheck

class Functor f => Apply f
  where
  zip :: f a -> f b -> f (a, b)

class Apply f => Applicative f
  where
  pure:: a -> f a

instance {-# OVERLAPPABLE #-} (Functor f, Applicative f) => A.Applicative f
  where
  (<*>) = (<*>)
  pure = pure

newtype BaseApplicative f a = BaseApplicative { runBaseApplicative :: f a }
  deriving (Functor, A.Applicative)

instance A.Applicative f => Apply (BaseApplicative f)
  where
  x `zip` y = A.liftA2 (,) x y

instance A.Applicative f => Applicative (BaseApplicative f)
  where
  pure = A.pure

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = uncurry f <$> fa `zip` fb

infixl 4 <*>
(<*>) :: Apply f => f (a -> b) -> f a -> f b
f <*> a = liftA2 ($) f a

infixl 4 *>
(*>) :: Apply f => f a -> f b -> f b
f *> a = (const id <$> f) <*> a

deriving via (BaseApplicative Maybe) instance Apply Maybe
deriving via (BaseApplicative Maybe) instance Applicative Maybe

deriving via (BaseApplicative []) instance Apply []
deriving via (BaseApplicative []) instance Applicative []

deriving via (BaseApplicative (StateT s m)) instance Monad m => Apply (StateT s m)
deriving via (BaseApplicative (StateT s m)) instance Monad m => Applicative (StateT s m)

instance (Apply m, Semigroup w) => Apply (WriterT w m)
  where
  WriterT x `zip` WriterT y = WriterT $ liftA2 (\(a, w1) (b, w2) -> ((a, b), w1 <> w2)) x y

instance (Applicative m, Monoid w) => Applicative (WriterT w m)
  where
  pure a = WriterT $ fmap (, mempty) $ pure a

deriving via (BaseApplicative IO) instance Apply IO
deriving via (BaseApplicative IO) instance Applicative IO

deriving via (BaseApplicative Gen) instance Apply Gen
deriving via (BaseApplicative Gen) instance Applicative Gen
