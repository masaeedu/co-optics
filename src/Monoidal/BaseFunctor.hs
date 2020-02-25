module Monoidal.BaseFunctor where

import MyPrelude

import qualified Control.Applicative as A

newtype BaseFunctor f a = BaseFunctor { runBaseFunctor :: f a }
  deriving (Functor, A.Applicative, A.Alternative)
