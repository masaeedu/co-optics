{-# LANGUAGE LambdaCase #-}
module Optics.Iso (module Optics.Iso, module Optics.Types) where

import MyPrelude

import Data.Bifunctor
import Data.Profunctor

import Optics.Types

-- Exchange
data Exchange a b s t = Exchange { _fwd :: s -> a, _bwd :: b -> t }

exch_id :: Exchange a b a b
exch_id = Exchange id id

exch_compose :: Exchange c d e f -> Exchange a b c d -> Exchange a b e f
exch_compose (Exchange f1 b1) (Exchange f2 b2) = Exchange  (f2 . f1) (b1 . b2)

instance Profunctor (Exchange a b)
  where
  dimap f g (Exchange fwd bwd) = Exchange (fwd . f) (g . bwd)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap

fwd :: Iso s t a b -> s -> a
fwd f = _fwd (f exch_id)

bwd :: Iso s t a b -> b -> t
bwd f = _bwd (f exch_id)

liftIso :: Functor f => Iso s t a b -> Iso (f s) (f t) (f a) (f b)
liftIso i = iso (fmap $ fwd i) (fmap $ bwd i)

liftIsoFirst :: Bifunctor f => Iso s t a b -> Iso (f s x) (f t x) (f a x) (f b x)
liftIsoFirst i = iso (first $ fwd i) (first $ bwd i)

uncons :: Iso [a] [b] (Maybe (a, [a])) (Maybe (b, [b]))
uncons = iso (\case { [] -> Nothing; (x : xs) -> Just (x, xs) }) (maybe [] (uncurry (:)))

maybeToEither :: Iso (Maybe a) (Maybe b) (Either () a) (Either () b)
maybeToEither = iso (maybe (Left ()) Right) (either (const Nothing) Just)

distinguish :: (a -> Bool) -> Iso a b (Either a a) (Either b b)
distinguish p = iso (\a -> if p a then Left a else Right a) (either id id)
