module Monoidal.Applicative where

import MyPrelude

import qualified Control.Applicative as A

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Data.Bifunctor

import Data.List.NonEmpty (NonEmpty(..))

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Hedgehog

import Monoidal.BaseFunctor

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

instance A.Applicative f => Apply (BaseFunctor f)
  where
  x `zip` y = A.liftA2 (,) x y

instance A.Applicative f => Applicative (BaseFunctor f)
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

deriving via (BaseFunctor Maybe) instance Apply Maybe
deriving via (BaseFunctor Maybe) instance Applicative Maybe

deriving via (BaseFunctor []) instance Apply []
deriving via (BaseFunctor []) instance Applicative []

deriving via (BaseFunctor (StateT s m)) instance Monad m => Apply (StateT s m)
deriving via (BaseFunctor (StateT s m)) instance Monad m => Applicative (StateT s m)

instance (Apply m, Semigroup w) => Apply (WriterT w m)
  where
  WriterT x `zip` WriterT y = WriterT $ liftA2 (\(a, w1) (b, w2) -> ((a, b), w1 <> w2)) x y

instance (Applicative m, Monoid w) => Applicative (WriterT w m)
  where
  pure a = WriterT $ fmap (, mempty) $ pure a

deriving via (BaseFunctor IO) instance Apply IO
deriving via (BaseFunctor IO) instance Applicative IO

deriving via (BaseFunctor Gen) instance Apply Gen
deriving via (BaseFunctor Gen) instance Applicative Gen

deriving via (BaseFunctor NonEmpty) instance Apply NonEmpty
deriving via (BaseFunctor NonEmpty) instance Applicative NonEmpty

deriving via (BaseFunctor (Either e)) instance Apply (Either e)
deriving via (BaseFunctor (Either e)) instance Applicative (Either e)

instance Semigroup a => Apply (Const a)
  where
  zip (Const x) (Const y) = Const $ x <> y

instance Monoid a => Applicative (Const a)
  where
  pure _ = Const mempty

deriving via (BaseFunctor Identity) instance Apply Identity
deriving via (BaseFunctor Identity) instance Applicative Identity

instance (Apply f, Apply g) => Apply (Compose f g)
  where
  zip (Compose f) (Compose g) = Compose $ fmap (uncurry zip) $ zip f g

instance (Applicative f, Applicative g) => Applicative (Compose f g)
  where
  pure = Compose . pure . pure
