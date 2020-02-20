module Profunctor.Kleisli where

import Control.Arrow (Kleisli(..))
import Control.Applicative (Alternative(..))
import Data.Profunctor (Cochoice(..))

instance (Monad m, Alternative m) => Cochoice (Kleisli m)
  where
  unleft (Kleisli amb) = Kleisli $ \a -> amb (Left a) >>= either pure (const empty)
