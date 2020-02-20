module Profunctor.Kleisli where

import Data.Profunctor (Profunctor (..), Strong(..), Choice(..), Cochoice(..))

import Filterable

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
