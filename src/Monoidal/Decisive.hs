module Monoidal.Decisive where

import MyPrelude

import Data.Void (Void)

class Functor f => Decide f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

class Decide f => Decisive f
  where
  vow :: f Void -> Void

instance Decide ((,) a)
  where
  decide (x, Left a) = Left (x, a)
  decide (x, Right b) = Right (x, b)
