module Prelude
  ( module P
  , module C
  , ifThenElse
  , type (+)
  , type (×)
  , module T
  , (&)
  )
  where

import "base" Prelude as P hiding
  ( Applicative(..)
  , zip
  , id
  , (.)
  )

import Control.Category as C

import Debug.Trace as T
import Data.Function ((&))

ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y = if b then x else y

type (+) = Either
infixr 6 +

type (×) = (,)
infixr 7 ×
