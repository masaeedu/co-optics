module Optics.Lens (module Optics.Lens, module Optics.Types) where

import MyPrelude

import Data.Profunctor

import Monoidal.Applicative
import Optics.Types

data Shop a b s t = Shop { _view :: s -> a, _update :: s -> b -> t }

shp_id :: Shop a b a b
shp_id = Shop id (const id)

instance Profunctor (Shop a b)
  where
  dimap f g (Shop v u) = Shop (v . f) (\s b -> g $ u (f s) b)

instance Strong (Shop a b)
  where
  first' (Shop v u) = Shop (v . fst) (\(a, c) b -> (u a b, c))

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens v u = dimap (\s -> (s, v s)) (uncurry u) . second'

view :: Lens s t a b -> s -> a
view l = _view $ l shp_id

update :: Lens s t a b -> s -> b -> t
update l = _update $ l shp_id

liftLens :: Apply f => Lens s t a b -> Lens (f s) (f t) (f a) (f b)
liftLens l = lens (fmap $ view l) (liftA2 $ update l)
