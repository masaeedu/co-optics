module Optics.Iso.Combinators where

import MyPrelude

import Data.Profunctor
import Data.Bifunctor

import Optics.Types

-- Exchange
data Exchange a b s t = Exchange { _fwd :: s -> a, _bwd :: b -> t }

exch_id :: Exchange a b a b
exch_id = Exchange id id

exch_compose :: Exchange c d e f -> Exchange a b c d -> Exchange a b e f
exch_compose (Exchange f1 b1) (Exchange f2 b2) = Exchange  (f2 . f1) (b1 . b2)

instance Profunctor (Exchange a b)
  where
  dimap f g (Exchange a b) = Exchange (a . f) (g . b)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

fwd :: Iso s t a b -> s -> a
fwd f = _fwd (f exch_id)

bwd :: Iso s t a b -> b -> t
bwd f = _bwd (f exch_id)

isoId :: Iso s a s a
isoId = iso id id

mapIso :: (Functor f, Functor g) => Iso s t a b -> Iso (f s) (g t) (f a) (g b)
mapIso i = iso (fmap $ fwd i) (fmap $ bwd i)

bimapIso :: Bifunctor f => Iso s t a b -> Iso s' t' a' b' -> Iso (f s s') (f t t') (f a a') (f b b')
bimapIso i j = iso (bimap (fwd i) (fwd j)) (bimap (bwd i) (bwd j))

firstIso :: Bifunctor f => Iso s t a b -> Iso (f s x) (f t y) (f a x) (f b y)
firstIso i = bimapIso i (iso id id)

distinguish :: (a -> Bool) -> Iso a b (a + a) (b + b)
distinguish p = iso (\a -> if p a then Left a else Right a) (either id id)
