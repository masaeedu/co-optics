module Optics.Common where

import Data.Functor.Identity
import Data.Profunctor

import Optics.Types

type LOptic p f s t a b = p a (f b) -> p s (f t)

convert :: Profunctor p => LOptic p Identity s t a b -> Optic p s t a b
convert pab = dimap id runIdentity . pab . dimap id Identity
