module Decisive where

class Functor f => Decisive f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

instance Decisive ((,) a)
  where
  decide (x, Left a) = Left (x, a)
  decide (x, Right b) = Right (x, b)
