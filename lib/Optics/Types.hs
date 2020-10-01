{-# LANGUAGE ImpredicativeTypes #-}
module Optics.Types where

import Data.Profunctor
import Profunctor.Mux
import Profunctor.Lazy

type Optic p  s t a b = p a b -> p s t

type Traversing p = (Visitor p, Choice p, Lazy2 p)

type Iso       s t a b = forall p. Profunctor p => Optic p s t a b
type Lens      s t a b = forall p. Strong     p => Optic p s t a b
type Colens    s t a b = forall p. Costrong   p => Optic p s t a b
type Prism     s t a b = forall p. Choice     p => Optic p s t a b
type Coprism   s t a b = forall p. Cochoice   p => Optic p s t a b
type Traversal s t a b = forall p. Traversing p => Optic p s t a b

type Optic'   p s a = Optic   p s s a a
type Iso'       s a = Iso       s s a a
type Lens'      s a = Lens      s s a a
type Colens'    s a = Colens    s s a a
type Prism'     s a = Prism     s s a a
type Coprism'   s a = Coprism   s s a a
type Traversal' s a = Traversal s s a a
