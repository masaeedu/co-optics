module MyPrelude
  ( module P
  , ifThenElse
  )
  where

import Prelude as P hiding
  ( Applicative(..)
  , zip
  )

ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y = if b then x else y
