{-# LANGUAGE TupleSections #-}

module Optics where

import Data.Profunctor (Profunctor(..), Strong(..), Costrong(..), Choice(..), Cochoice(..))
import Data.Bifunctor (first, second)
import Control.Applicative (liftA2)

import Decisive
import Profunctor.Re

type Optic p  s t a b = p a b -> p s t

type Iso      s t a b = forall p. Profunctor p => Optic p s t a b
type Lens     s t a b = forall p. Strong     p => Optic p s t a b
type Colens   s t a b = forall p. Costrong   p => Optic p s t a b
type Prism    s t a b = forall p. Choice     p => Optic p s t a b
type Coprism  s t a b = forall p. Cochoice   p => Optic p s t a b

type Optic' p s a = Optic p s s a a
type Iso'     s a = Iso     s s a a
type Lens'    s a = Lens    s s a a
type Colens'  s a = Colens  s s a a
type Prism'   s a = Prism   s s a a
type Coprism' s a = Coprism s s a a

-- Tensorial structure
assocE :: Iso' (Either a (Either b c)) (Either (Either a b) c)
assocE = dimap
  (either (Left . Left) (either (Left . Right) Right))
  (either (either Left (Right . Left)) (Right . Right))

symmE :: Iso' (Either a b) (Either b a)
symmE = dimap
  (either Right Left)
  (either Right Left)

-- Concrete optics and conversions

-- Isomorphisms
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

-- Lenses
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

liftLens :: Applicative f => Lens s t a b -> Lens (f s) (f t) (f a) (f b)
liftLens l = lens (fmap $ view l) (liftA2 $ update l)

-- Prisms
data Market a b s t = Market { _build :: b -> t, _match :: s -> Either t a }

mkt_id :: Market a b a b
mkt_id = Market id Right

mkt_compose :: Market c d e f -> Market a b c d -> Market a b e f
mkt_compose (Market b2 m2) (Market b1 m1) = Market (b2 . b1) (either Left (either (Left . b2) Right) . fmap m1 . m2)

instance Profunctor (Market a b)
  where
  dimap f g (Market b m) = Market (g . b) (first g . m . f)

instance Choice (Market a b) where
  left' (Market b m) = Market (Left . b) (fwd assocE . second (fwd symmE) . fwd (re assocE) . first m)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build match = dimap match (either id build) . right'

build :: Prism s t a b -> b -> t
build p = _build $ p mkt_id

match :: Prism s t a b -> s -> Either t a
match p = _match $ p mkt_id

liftPrism :: Decisive f => Prism s t a b -> Prism (f s) (f t) (f a) (f b)
liftPrism p = prism (fmap $ build p) (decide . fmap (match p))

-- Reversals
re :: Optic (Re p a b) s t a b -> Optic p b a t s
re p = runRe $ p $ Re id
