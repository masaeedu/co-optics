module Profunctor.Re where

import Data.Profunctor (Profunctor(..), Strong(..), Costrong(..), Choice(..), Cochoice(..))

newtype Re p s t a b = Re { runRe :: p b a -> p t s }

instance Profunctor p => Profunctor (Re p s t)
  where
  dimap f g (Re p) = Re $ p . dimap g f

instance Choice p => Cochoice (Re p s t)
  where
  unleft (Re p) = Re $ p . left'

instance Cochoice p => Choice (Re p s t)
  where
  left' (Re p) = Re $ p . unleft

instance Strong p => Costrong (Re p s t)
  where
  unfirst (Re p) = Re $ p . first'

instance Costrong p => Strong (Re p s t)
  where
  first' (Re p) = Re $ p . unfirst
