module Optics.Reverse where

import Data.Biprofunctor.Upstream (Upstream(..))
import qualified Control.Category.Product as CP
import Optics.Types

re :: Optic (Upstream (Optic p) a b) a b s t -> Optic p t s b a
re (Optic o) = runUpstream $ o CP.biid
