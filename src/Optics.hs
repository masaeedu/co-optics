{-# LANGUAGE LambdaCase #-}
module Optics where

import MyPrelude

import GHC.Natural

import Data.Profunctor (Profunctor(..), Strong(..), Costrong(..), Choice(..), Cochoice(..))
import Data.Bifunctor (first, second)
import Data.Functor.Identity (Identity(..))

import Data.List.NonEmpty (NonEmpty(..), toList)

import Data.Digit (DecDigit, charDecimal, _NaturalDigits)

import Monoidal.Applicative
import Monoidal.Decisive
import Monoidal.Filterable

import Profunctor.Joker
import Profunctor.Re
import Profunctor.Kleisli
import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy

type Optic p  s t a b = p a b -> p s t

type Iso       s t a b = forall p. Profunctor p => Optic p s t a b
type Lens      s t a b = forall p. Strong     p => Optic p s t a b
type Colens    s t a b = forall p. Costrong   p => Optic p s t a b
type Prism     s t a b = forall p. Choice     p => Optic p s t a b
type Coprism   s t a b = forall p. Cochoice   p => Optic p s t a b
type Traversal s t a b = forall p. Mux        p => Optic p s t a b

type Optic'   p s a = Optic   p s s a a
type Iso'       s a = Iso       s s a a
type Lens'      s a = Lens      s s a a
type Colens'    s a = Colens    s s a a
type Prism'     s a = Prism     s s a a
type Coprism'   s a = Coprism   s s a a
type Traversal' s a = Traversal s s a a

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

liftLens :: Apply f => Lens s t a b -> Lens (f s) (f t) (f a) (f b)
liftLens l = lens (fmap $ view l) (liftA2 $ update l)

uncons :: Iso [a] [b] (Maybe (a, [a])) (Maybe (b, [b]))
uncons = iso (\case { [] -> Nothing; (x : xs) -> Just (x, xs) }) (maybe [] (uncurry (:)))

m2e :: Iso (Maybe a) (Maybe b) (Either () a) (Either () b)
m2e = iso (maybe (Left ()) Right) (either (const Nothing) Just)

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
prism b m = dimap m (either id b) . right'

build :: Prism s t a b -> b -> t
build p = _build $ p mkt_id

match :: Prism s t a b -> s -> Either t a
match p = _match $ p mkt_id

liftPrism :: Decide f => Prism s t a b -> Prism (f s) (f t) (f a) (f b)
liftPrism p = prism (fmap $ build p) (decide . fmap (match p))

predicate :: (a -> Bool) -> Prism' a a
predicate f = prism id (\x -> if f x then Right x else Left x)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

c2d :: Prism' Char DecDigit
c2d = convert charDecimal

digits2int :: Prism' (NonEmpty DecDigit) Natural
digits2int = convert _NaturalDigits

asNonEmpty :: Prism [a] [b] (NonEmpty a) (NonEmpty b)
asNonEmpty = prism toList (\case { [] -> Left []; (x : xs) -> Right $ x :| xs })

bounded ::  Int -> Int -> Prism' Int Int
bounded l h = predicate (\i -> i <= h && i >= l)

-- Traversals
newtype Bazaar a b s t = Bazaar { runBazaar :: forall f. Applicative f => (a -> f b) -> s -> f t }

bz_id :: Bazaar a b a b
bz_id = Bazaar id

bz_compose :: Bazaar c d e f -> Bazaar a b c d -> Bazaar a b e f
bz_compose (Bazaar x) (Bazaar y) = Bazaar $ x . y

instance Profunctor (Bazaar a b)
  where
  dimap f g (Bazaar b) = Bazaar $ dimap id (dimap f $ fmap g) $ b

instance Strong (Bazaar a b)
  where
  first' (Bazaar bz) = Bazaar $ \f (a, c) -> fmap (, c) (bz f a)

instance Choice (Bazaar a b)
  where
  left' (Bazaar bz) = Bazaar $ \f -> either (fmap Left . bz f) (pure . Right)

instance Mux (Bazaar a b)
  where
  Bazaar x /\ Bazaar y = Bazaar $ \f (a, c) -> x f a `zip` y f c

instance Visitor (Bazaar a b)
  where
  start = Bazaar $ \_ _ -> pure ()

instance Functor (Bazaar a b s)
  where
  fmap ab (Bazaar b) = Bazaar $ (fmap ab .) . b

instance Apply (Bazaar a b s)
  where
  Bazaar a `zip` Bazaar b = Bazaar $ \f s -> a f s `zip` b f s

instance Applicative (Bazaar a b s)
  where
  pure a = Bazaar $ \_ _ -> pure a

each :: (Demux p, Visitor p, Lazy2 p) => Optic p [a] [b] a b
each pab = uncons $ m2e $ dimap (either Right Left) (either Right Left) $ (\/ start) $ (pab /\) $ defer $ \_ -> each pab

-- Reversals
re :: Optic (Re p a b) s t a b -> Optic p b a t s
re p = runRe $ p $ Re id

-- Coprisms
whittle :: Filterable f => Coprism s t a b -> f b -> f t
whittle p f = runJoker $ p $ Joker $ f

speculate :: Filterable f => Coprism s t a b -> (a -> f b) -> s -> f t
speculate = dimap Kleisli runKleisli

-- Conversions from lens library
type LOptic p f s t a b = p a (f b) -> p s (f t)

convert :: Profunctor p => LOptic p Identity s t a b -> Optic p s t a b
convert pab = dimap id runIdentity . pab . dimap id Identity
