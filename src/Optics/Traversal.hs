{-# LANGUAGE PartialTypeSignatures #-}
module Optics.Traversal (module Optics.Traversal, module Optics.Types) where

import MyPrelude

import Data.Profunctor
import Data.List.NonEmpty (NonEmpty)

import Control.Monad

import Monoidal.Applicative

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy

import Optics.Types
import Optics.Iso
import Optics.Reverse

import SOP.EOT

newtype Bazaar a b s t = Bazaar { runBazaar :: forall f. (Applicative f, Monad f) => (a -> f b) -> s -> f t }

bz_id :: Bazaar a b a b
bz_id = Bazaar id

bz_compose :: Bazaar c d e f -> Bazaar a b c d -> Bazaar a b e f
bz_compose (Bazaar x) (Bazaar y) = Bazaar $ x . y

instance Category (Bazaar a b)
  where
  id = Bazaar $ const pure
  Bazaar f . Bazaar g = Bazaar $ \afb -> f afb <=< g afb

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
  rec = (pab /\ (defer $ \_ -> rec)) \/ pab & unconsNonEmpty

many :: (Demux p, Visitor p, Lazy2 p) => Optic p [a] [b] a b
many pab = some pab \/ start
  & symmE & maybeToEither & listToNonEmpty

separated :: (Demux p, Mux p, Lazy2 p) => p () x -> Optic p (NonEmpty a) (NonEmpty b) a b
separated s v = rec
  where
  rec = (v /\ s \\ (defer $ \_ -> rec)) \/ v & unconsNonEmpty
