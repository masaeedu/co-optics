module Profunctor.Joker where

import Data.Profunctor (Cochoice(..))
import Data.Bifunctor.Joker (Joker(..))

import Filterable

instance (Filterable m) => Cochoice (Joker m)
  where
  unleft (Joker m) = Joker $ fst $ partition m
