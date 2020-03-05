module Monoidal.Decisive where

import MyPrelude

import Data.Void

class Functor f => Decide f
  where
  decide :: f (a + b) -> f a + f b

class Decide f => Decisive f
  where
  vow :: f Void -> Void

instance Decide ((,) a)
  where
  decide (x, Left a) = Left (x, a)
  decide (x, Right b) = Right (x, b)

instance Decisive ((,) a)
  where
  vow (_, a) = absurd a

instance Decide (Either e)
  where
  decide (Left e) = Left $ Left e
  decide (Right (Left a)) = Left $ Right a
  decide (Right (Right b)) = Right $ Right b
