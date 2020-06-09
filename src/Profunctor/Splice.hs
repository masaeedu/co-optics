module Profunctor.Splice where

import Data.Profunctor

import MyPrelude

class Profunctor p => Splice p
  where
  splice :: p a b -> p c d -> p (a + c) (b × d)
