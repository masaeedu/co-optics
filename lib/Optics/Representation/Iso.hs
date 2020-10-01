module Optics.Representation.Iso where

import Data.Profunctor (Profunctor(..))

import Data.Biprofunctor (Biprofunctor(..))
import Data.Biprofunctor.Downstream (Downstream(..))

import qualified Control.Category.Product as CP

import Optics.Types (Optic(..), Iso)

-- {{{ Type

data Exchange a b s t = Exchange { _fwd :: s -> a, _bwd :: b -> t }

-- }}}

-- {{{ Instances

instance Biprofunctor Exchange
  where
  bidimap f g h i (Exchange fwd bwd) = Exchange (f . fwd . h) (i . bwd . g)

deriving via (Downstream Exchange a b) instance Profunctor (Exchange a b)

instance CP.ProductCategory Exchange
  where
  biid = Exchange id id
  Exchange f1 b1 `bicompose` Exchange f2 b2 = Exchange (f2 . f1) (b1 . b2)

-- }}}

-- {{{ Combinators

toIso :: Exchange a b s t -> Iso a b s t
toIso (Exchange f b) = Optic $ dimap f b

fromIso :: Iso a b s t -> Exchange a b s t
fromIso i = runOptic i CP.biid


-- }}}
