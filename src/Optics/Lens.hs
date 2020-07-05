{-# LANGUAGE ViewPatterns #-}
module Optics.Lens (module Optics.Lens, module Optics.Types) where
import MyPrelude

import Data.Profunctor
import Data.Void

import Monoidal.Applicative

import Control.Arrow ((&&&), (***))
import Profunctor.Demux
import Profunctor.Mux

import Optics.Types

data Shop a b s t = Shop { _view :: s -> a, _update :: s -> b -> t }

shp_id :: Shop a b a b
shp_id = Shop id (const id)

shp_compose :: Shop c d e f -> Shop a b c d -> Shop a b e f
shp_compose (Shop v1 u1) (Shop v2 u2) = Shop (v2 . v1) (\e b -> u1 e $ u2 (v1 e) b)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens v u = dimap (\s -> (s, v s)) (uncurry u) . second'

toLens :: Shop a b s t -> Lens s t a b
toLens (Shop v u) = lens v u

fromLens :: Lens s t a b -> Shop a b s t
fromLens p = p shp_id

view :: Lens s t a b -> s -> a
view l = _view $ fromLens l

update :: Lens s t a b -> s -> b -> t
update l = _update $ fromLens l

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

liftLens :: Apply f => Lens s t a b -> Lens (f s) (f t) (f a) (f b)
liftLens l = lens (fmap $ view l) (liftA2 $ update l)

altLens :: Lens s t a b -> Lens s' t' a b -> Lens (s + s') (t + t') a b
altLens (fromLens -> s1) (fromLens -> s2) = toLens $ s1 \/ s2

instance Semigroup a => Mux (Shop a b)
  where
  Shop v1 u1 /\ Shop v2 u2 = Shop (\(x, y) -> v1 x <> v2 y) (\(x, y) b -> (u1 x b, u2 y b))

muxLens :: Semigroup a => Lens s t a b -> Lens s' t' a b -> Lens (s × s') (t × t') a b
muxLens l1 l2 = toLens $ fromLens l1 /\ fromLens l2

instance Monoid a => Visitor (Shop a b)
  where
  start = Shop (const mempty) (const $ const $ ())

startLens :: Monoid a => Lens s () a b
startLens = toLens start

rmux :: Semigroup t => Shop a b s t -> Shop a' b' s t -> Shop (a × a') (b × b') s t
rmux (Shop v1 u1) (Shop v2 u2) = Shop (v1 &&& v2) (\s -> uncurry (<>) . (u1 s *** u2 s))

rstart :: Monoid t => Shop () b s t
rstart = Shop (const ()) (const $ const $ mempty)

emptyLens :: Lens Void x a b
emptyLens = toLens stop
