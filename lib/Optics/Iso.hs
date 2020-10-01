module Optics.Iso where

import Data.Void

import Data.Profunctor (Profunctor(..))

import Data.Bifunctor (Bifunctor(..))

import Control.Category.Product as CP
import qualified Control.Category.Iso as CI
import qualified Control.Category.Tensor as T

import Optics.Types
import Optics.Representation.Iso (Exchange(..))
import Optics.Reverse (re)

-- {{{ Construct
iso :: (s -> a) -> (b -> t) -> Iso a b s t
iso f g = Optic $ dimap f g
-- }}}

-- {{{ Deconstruct
fwd :: Iso a b s t -> s -> a
fwd f = _fwd (runOptic f CP.biid)

bwd :: Iso a b s t -> b -> t
bwd f = _bwd (runOptic f CP.biid)
-- }}}

-- {{{ Interop
type Bijection = CI.Iso (->)

fromBijection :: Bijection a s -> Bijection b t -> Iso a b s t
fromBijection (CI.Iso _ b) (CI.Iso f _) = iso b f

fromBijection' :: Bijection a s -> Iso' a s
fromBijection' (CI.Iso f b) = iso b f
-- }}}

-- {{{ Functors
mapIso :: (Functor f, Functor g) => Iso a b s t -> Iso (f a) (g b) (f s) (g t)
mapIso i = iso (fmap $ fwd i) (fmap $ bwd i)

bimapIso :: Bifunctor f => Iso a b s t -> Iso a' b' s' t' -> Iso (f a a') (f b b') (f s s') (f t t')
bimapIso i j = iso (bimap (fwd i) (fwd j)) (bimap (bwd i) (bwd j))

firstIso :: Bifunctor f => Iso a b s t -> Iso (f a x) (f b y) (f s x) (f t y)
firstIso i = bimapIso i (iso id id)
-- }}}

-- {{{ Combinators
distinguish :: (a -> Bool) -> Iso (a + a) (b + b) a b
distinguish p = iso (\a -> if p a then Left a else Right a) (either id id)
-- }}}

-- {{{ Tensors
assoc ::
  ( T.Arrow t ~ (->)
  , T.Associative t
  , T.Ask t a
  , T.Ask t b
  , T.Ask t c
  , T.Ask t d
  , T.Ask t e
  , T.Ask t f
  ) =>
  Iso ((a `t` b) `t` c) ((d `t` e) `t` f) (a `t` (b `t` c)) (d `t` (e `t` f))
assoc = fromBijection T.assoc T.assoc

symm ::
  ( T.Arrow t ~ (->)
  , T.Symmetric t
  , T.Ask t a
  , T.Ask t b
  , T.Ask t c
  , T.Ask t d
  ) =>
  Iso (a `t` b) (c `t` d) (b `t` a) (d `t` c)
symm = iso T.symm T.symm

runit :: forall t a b.
  ( T.Arrow t ~ (->)
  , T.Unital t
  , T.Ask t a
  , T.Ask t b
  ) =>
  Iso a b (a `t` T.Unit t) (b `t` T.Unit t)
runit = re $ fromBijection (T.runit @t) (T.runit @t)

lunit :: forall t a b.
  ( T.Arrow t ~ (->)
  , T.Unital t
  , T.Ask t a
  , T.Ask t b
  ) =>
  Iso a b (T.Unit t `t` a) (T.Unit t `t` b)
lunit = re $ fromBijection (T.lunit @t) (T.lunit @t)

ldistrib ::
  ( T.Arrow times ~ (->)
  , T.Arrow plus ~ (->)
  , T.LRig times plus
  , T.Ask times a
  , T.Ask plus  a
  , T.Ask times b
  , T.Ask plus  b
  , T.Ask times c
  , T.Ask plus  c
  , T.Ask times d
  , T.Ask plus  d
  , T.Ask times e
  , T.Ask plus  e
  , T.Ask times f
  , T.Ask plus  f
  ) =>
  Iso
    ((a `times` b) `plus` (a `times` c))
    ((d `times` e) `plus` (d `times` f))
    (a `times` (b `plus` c))
    (d `times` (e `plus` f))
ldistrib = iso T.ldistrib T.opldistrib

rdistrib ::
  ( T.Arrow times ~ (->)
  , T.Arrow plus ~ (->)
  , T.RRig times plus
  , T.Ask times a
  , T.Ask plus  a
  , T.Ask times b
  , T.Ask plus  b
  , T.Ask times c
  , T.Ask plus  c
  , T.Ask times d
  , T.Ask plus  d
  , T.Ask times e
  , T.Ask plus  e
  , T.Ask times f
  , T.Ask plus  f
  ) =>
  Iso
    ((b `times` a) `plus` (c `times` a))
    ((e `times` d) `plus` (f `times` d))
    ((b `plus` c) `times` a)
    ((e `plus` f) `times` d)
rdistrib = iso T.rdistrib T.oprdistrib
-- }}}

-- {{{ Tensors (monomorphized)
assocE :: Iso ((a + b) + c) ((d + e) + f) (a + (b + c)) (d + (e + f))
assocE = assoc

symmE :: Iso (a + b) (c + d) (b + a) (d + c)
symmE = symm

runitE :: Iso a b (a + Void) (b + Void)
runitE = runit @(+)

lunitE :: Iso a b (Void + a) (Void + b)
lunitE = lunit @(+)

assocT :: Iso ((a × b) × c) ((d × e) × f) (a × (b × c)) (d × (e × f))
assocT = assoc

symmT :: Iso (b × a) (d × c) (a × b) (c × d)
symmT = symm

runitT :: Iso a b (a × ()) (b × ())
runitT = runit @(×)

lunitT :: Iso a b (() × a) (() × b)
lunitT = lunit @(×)

ldistribT :: Iso (a × b + a × c) (d × e + d × f) (a × (b + c)) (d × (e + f))
ldistribT = ldistrib

rdistribT :: Iso (b × a + c × a) (e × d + f × d) ((b + c) × a) ((e + f) × d)
rdistribT = rdistrib
-- }}}

-- {{{ Data structures
uncons :: Iso (Maybe (a, [a])) (Maybe (b, [b])) [a] [b]
uncons = iso (\case { [] -> Nothing; (x : xs) -> Just (x, xs) }) (maybe [] (uncurry (:)))

maybeToEither :: Iso (() + a) (() + b) (Maybe a) (Maybe b)
maybeToEither = iso (maybe (Left ()) Right) (either (const Nothing) Just)
-- }}}

