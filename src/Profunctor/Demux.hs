module Profunctor.Demux where

import MyPrelude

import Data.Profunctor
import Data.Bifunctor.Joker
import Data.Bifunctor.Product
import Data.Void

import Profunctor.Kleisli
import Monoidal.Alternative

class Profunctor p => Demux p
  where
  (\/) ::  p a b -> p c d -> p (Either a c) (Either b d)

class Demux p => Switch p
  where
  stop :: p Void a

instance Functor m => Demux (Kleisli m)
  where
  Kleisli f \/ Kleisli g = Kleisli $ either (fmap Left . f) (fmap Right . g)

instance Functor m => Switch (Kleisli m)
  where
  stop = Kleisli $ absurd

instance Alt m => Demux (Joker m)
  where
  Joker x \/ Joker y = Joker $ x <|> y

instance Alternative m => Switch (Joker m)
  where
  stop = Joker $ empty

instance (Demux p, Demux q) => Demux (Product p q)
  where
  Pair a b \/ Pair c d = Pair (a \/ c) (b \/ d)

instance (Switch p, Switch q) => Switch (Product p q)
  where
  stop = Pair stop stop
