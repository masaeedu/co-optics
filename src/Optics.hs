module Optics where

import Data.Profunctor (Profunctor(..), Strong(..), Costrong(..), Choice(..), Cochoice(..))

import Profunctor.Re

type Optic p  s t a b = p a b -> p s t

type Iso      s t a b = forall p. Profunctor p => Optic p s t a b
type Lens     s t a b = forall p. Strong     p => Optic p s t a b
type Colens   s t a b = forall p. Costrong   p => Optic p s t a b
type Prism    s t a b = forall p. Choice     p => Optic p s t a b
type Coprism  s t a b = forall p. Cochoice   p => Optic p s t a b

type Iso'     s a = Iso     s s a a
type Lens'    s a = Lens    s s a a
type Colens'  s a = Colens  s s a a
type Prism'   s a = Prism   s s a a
type Coprism' s a = Coprism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build match = dimap match (either id build) . right'

re :: Optic (Re p a b) s t a b -> Optic p b a t s
re p = runRe $ p $ Re id
