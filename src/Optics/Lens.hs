{-# LANGUAGE ViewPatterns #-}
module Optics.Lens (module Optics.Lens, module Optics.Types) where
import MyPrelude

import Data.Profunctor
import Data.Void

import Monoidal.Applicative
import Profunctor.Demux
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

instance Demux (Shop a b)
  where
  Shop v1 u1 \/ Shop v2 u2 = Shop (either v1 v2) (either ((Left .) . u1) ((Right .) . u2))

instance Switch (Shop a b)
  where
  stop = Shop absurd absurd

toLens :: Shop a b s t -> Lens s t a b
toLens (Shop v u) = lens v u

fromLens :: Lens s t a b -> Shop a b s t
fromLens p = p shp_id

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens v u = dimap (\s -> (s, v s)) (uncurry u) . second'

view :: Lens s t a b -> s -> a
view l = _view $ fromLens l

update :: Lens s t a b -> s -> b -> t
update l = _update $ fromLens l

liftLens :: Apply f => Lens s t a b -> Lens (f s) (f t) (f a) (f b)
liftLens l = lens (fmap $ view l) (liftA2 $ update l)

altLens :: Lens s t a b -> Lens s' t' a b -> Lens (s + s') (t + t') a b
altLens (fromLens -> s1) (fromLens -> s2) = toLens $ s1 \/ s2

emptyLens :: Lens Void Void a b
emptyLens = toLens stop
