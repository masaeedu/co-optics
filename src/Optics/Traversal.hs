{-# LANGUAGE PartialTypeSignatures #-}
module Optics.Traversal (module Optics.Traversal, module Optics.Types, module B) where

import MyPrelude

import Data.Profunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Void

import Data.FunList.Bazaar
import qualified Data.FunList.Bazaar as B (each, wander, traverseOf)

import Monoidal.Applicative

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy

import Optics.Types
import Optics.Iso

bz_id :: Traversal1 a b a b
bz_id = T1 $ singleton

bz_compose :: Traversal1 c d e f -> Traversal1 a b c d -> Traversal1 a b e f
bz_compose (BTraversal x) (BTraversal y) = BTraversal $ x . y

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
