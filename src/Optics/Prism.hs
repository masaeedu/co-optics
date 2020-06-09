{-# LANGUAGE DeriveGeneric, ViewPatterns #-}
module Optics.Prism (module Optics.Prism, module Optics.Types) where

import MyPrelude

import GHC.Generics
import GHC.Natural
import Data.Char
import Data.Bifunctor
import Data.Profunctor
import Data.List.NonEmpty (NonEmpty)

import Control.Arrow ((&&&))

import Data.Digit
import Data.Generics.Wrapped (_Wrapped)

import Monoidal.Decisive
import Monoidal.Filterable

import Profunctor.Joker
import Profunctor.Kleisli
import Profunctor.Mux

import Optics.Types
import Optics.Iso
import Optics.Reverse
import Optics.Common

import SOP.EOT

data Market a b s t = Market { _build :: b -> t, _match :: s -> Either t a }

mkt_id :: Market a b a b
mkt_id = Market id Right

mkt_compose :: Market c d e f -> Market a b c d -> Market a b e f
mkt_compose (Market b2 m2) (Market b1 m1) = Market (b2 . b1) (either Left (either (Left . b2) Right) . fmap m1 . m2)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism b m = dimap m (either id b) . right'

toPrism :: Market a b s t -> Prism s t a b
toPrism (Market b m) = prism b m

fromPrism :: Prism s t a b -> Market a b s t
fromPrism p = p mkt_id

build :: Prism s t a b -> b -> t
build p = _build $ p mkt_id

match :: Prism s t a b -> s -> Either t a
match p = _match $ p mkt_id

instance Profunctor (Market a b)
  where
  dimap f g (Market b m) = Market (g . b) (first g . m . f)

instance Choice (Market a b)
  where
  left' (Market b m) = Market (Left . b) (fwd assocE . second (fwd symmE) . fwd (re assocE) . first m)

instance Mux (Market a b)
  where
  Market b1 m1 /\ Market b2 m2 = Market (b1 &&& b2) (\(a, c) -> liftLeft (,) (m1 a) (m2 c))
    where
    liftLeft f (Left a)  (Left b)  = Left $ f a b
    liftLeft _ (Right x) _         = Right x
    liftLeft _ _         (Right x) = Right x

instance Visitor (Market a b)
  where
  start = Market (const ()) (const $ Left ())

liftPrism :: Decide f => Prism s t a b -> Prism (f s) (f t) (f a) (f b)
liftPrism p = prism (fmap $ build p) (decide . fmap (match p))

zipPrism :: Prism s t a b -> Prism s' t' a b -> Prism (s × s') (t × t') a b
zipPrism (fromPrism -> m1) (fromPrism -> m2) = toPrism $ m1 /\ m2

defaultPrism :: Prism x () a b
defaultPrism = toPrism start

predicate :: (a -> Bool) -> Prism' a a
predicate f = prism id (\x -> if f x then Right x else Left x)

exactly :: Eq a => a -> Prism' a a
exactly a = predicate (== a)

among :: Eq a => [a] -> Prism' a a
among as = predicate (`elem` as)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = gsop . right'

_Nothing :: Prism' (Maybe a) ()
_Nothing = gsop . left'

c2d :: Prism' Char DecDigit
c2d = convert charDecimal

digitsAsNatural :: Prism' (NonEmpty DecDigit) Natural
digitsAsNatural = convert _NaturalDigits

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

-- Coprisms
whittle :: Filterable f => Coprism s t a b -> f b -> f t
whittle p f = runJoker $ p $ Joker $ f

speculate :: Filterable f => Coprism s t a b -> (a -> f b) -> s -> f t
speculate = dimap Kleisli runKleisli
