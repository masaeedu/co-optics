module Profunctor.Kleisli where

import MyPrelude

import qualified Prelude as P

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

instance P.Applicative m => P.Applicative (Kleisli m a)
  where
  Kleisli f <*> Kleisli a = Kleisli $ \x -> f x P.<*> a x
  pure a = Kleisli $ const $ P.pure a

instance Monad m => Monad (Kleisli m a)
  where
  return = P.pure
  Kleisli f >>= amb = Kleisli $ \x -> f x >>= ($ x) . runKleisli . amb
