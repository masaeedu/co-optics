module Optics.Reverse (module Optics.Reverse, module Optics.Types) where

import MyPrelude

import Profunctor.Re

import Optics.Types

re :: Optic (Re p a b) s t a b -> Optic p b a t s
re p = runRe $ p $ Re id
