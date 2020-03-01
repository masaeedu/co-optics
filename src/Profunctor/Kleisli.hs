module Profunctor.Kleisli where

import MyPrelude

import Data.Profunctor (Profunctor (..), Strong(..), Choice(..), Cochoice(..))

import Monoidal.Filterable
import Monoidal.Applicative

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Functor m => Profunctor (Kleisli m)
  where
  dimap f g (Kleisli amb) = Kleisli $ fmap g . amb . f

instance Functor f => Strong (Kleisli f)
  where
  first' (Kleisli amb) = Kleisli $ \(a, c) -> fmap (, c) $ amb a

instance Applicative f => Choice (Kleisli f)
  where
  left' (Kleisli amb) = Kleisli $ either (fmap Left . amb) (fmap Right . pure)

instance Filterable m => Cochoice (Kleisli m)
  where
  unleft (Kleisli amb) = Kleisli $ fst . partition . amb . Left

instance Functor m => Functor (Kleisli m a)
  where
  fmap f (Kleisli amb) = Kleisli $ fmap f . amb

instance Apply m => Apply (Kleisli m a)
  where
  Kleisli f `zip` Kleisli a = Kleisli $ \x -> f x `zip` a x

instance Applicative m => Applicative (Kleisli m a)
  where
  pure a = Kleisli $ const $ pure a

instance (Applicative m, Monad m) => Monad (Kleisli m a)
  where
  return = pure
  Kleisli f >>= amb = Kleisli $ \x -> f x >>= ($ x) . runKleisli . amb
