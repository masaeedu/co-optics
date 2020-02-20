module Profunctor.Joker where

import Data.Profunctor (Cochoice(..), Strong(..), Costrong(..))
import Data.Bifunctor.Joker (Joker(..))

import Filterable

instance (Filterable m) => Cochoice (Joker m)
  where
  unleft (Joker m) = Joker $ fst $ partition m

instance Functor m => Costrong (Joker m)
  where
  unfirst (Joker m) = Joker $ fst <$> m
