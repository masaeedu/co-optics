{-# LANGUAGE LambdaCase, DeriveGeneric, ViewPatterns #-}
module Optics (module Optics, module Optics.Types, module Optics.Iso) where

import MyPrelude

import GHC.Generics
import GHC.Natural

import Data.Char (readLitChar, showLitChar)
import Data.Profunctor (Profunctor(..), Strong(..), Choice(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Identity (Identity(..))

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Generics.Wrapped (_Wrapped)

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

import Optics.Types
import Optics.Iso

import SOP.EOT

-- Tensorial structure
assocE :: Iso' (Either a (Either b c)) (Either (Either a b) c)
assocE = dimap
  (either (Left . Left) (either (Left . Right) Right))
  (either (either Left (Right . Left)) (Right . Right))

symmE :: Iso (Either a b) (Either c d) (Either b a) (Either d c)
symmE = dimap
  (either Right Left)
  (either Right Left)

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

exactly :: Eq a => a -> Prism' a a
exactly a = predicate (== a)

among :: Eq a => [a] -> Prism' a a
among as = predicate (`elem` as)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

c2d :: Prism' Char DecDigit
c2d = convert charDecimal

digitsAsNatural :: Prism' (NonEmpty DecDigit) Natural
digitsAsNatural = convert _NaturalDigits

asNonEmpty :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
asNonEmpty = gsop . re maybeToEither . liftIso (re gsop)

bounded ::  Int -> Int -> Prism' Int Int
bounded l h = predicate (\i -> i <= h && i >= l)

i2n :: Prism' Int Natural
i2n = prism naturalToInt (\i -> if i < 0 then Left i else Right (intToNatural i))

data Escape = Escape Char
  deriving (Generic, Show)

-- Convert a character to its escape code
asEscapeCode :: Prism' Char Escape
asEscapeCode = convert _Wrapped >>> prism
  (\c -> fst $ head $ readLitChar $ ['\\', c])
  (\c -> case showLitChar c "" of
    { ('\\' : c' : _) -> Right $ c'
    ; _ -> Left c
    })

unconsed :: Iso (NonEmpty a) (NonEmpty b) ((a, NonEmpty a) + a) ((b, NonEmpty b) + b)
unconsed = iso f b
  where
  f (NE.uncons -> (x, xs)) = case xs of { Nothing -> Right x; Just xs' -> Left (x, xs') }
  b = either (uncurry NE.cons) pure

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

some :: (Mux p, Demux p, Lazy2 p) => Optic p (NonEmpty a) (NonEmpty b) a b
some pab = rec
  where
  rec = unconsed $ (pab /\ (defer $ \_ -> rec)) \/ pab

many :: (Demux p, Visitor p, Lazy2 p) => Optic p [a] [b] a b
many pab = asNonEmpty $ maybeToEither $ symmE $ some pab \/ start

separated :: (Demux p, Mux p, Lazy2 p) => p () x -> Optic p (NonEmpty a) (NonEmpty b) a b
separated s v = rec
  where
  rec = unconsed $ (v /\ s \\ (defer $ \_ -> rec)) \/ v

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
