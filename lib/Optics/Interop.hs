module Optics.Interop where

import Data.Functor.Identity (Identity(..))

import Data.Profunctor (Profunctor(..))

import Optics.Types (Optic(..))

type LOptic p f a b s t = p a (f b) -> p s (f t)

convert :: Profunctor p => LOptic p Identity a b s t -> Optic p a b s t
convert pab = Optic $ dimap id runIdentity . pab . dimap id Identity
