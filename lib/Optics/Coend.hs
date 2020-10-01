module Optics.Coend where

data COptic m1 m2 a b s t = forall x. COptic { implicate :: s -> m1 x a, extricate :: m2 x b -> t }
