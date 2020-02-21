module MyPrelude
  ( module P
  , ifThenElse
  , module T
  , (&)
  )
  where

import Prelude as P hiding
  ( Applicative(..)
  , zip
  )

import Debug.Trace as T
import Data.Function ((&))

ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y = if b then x else y
