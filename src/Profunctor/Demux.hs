module Profunctor.Demux where

import MyPrelude

import Data.Profunctor
import Data.Bifunctor.Product
import Data.Void

import Profunctor.Joker
import Profunctor.Kleisli
import Monoidal.Alternative

class Profunctor p => Demux p
  where
  (\/) ::  p a b -> p c d -> p (a + c) (b + d)

infixr 5 \/

discard :: a -> ()
discard = const ()

(\\//) :: Demux p => p a b -> p c b -> p (a + c) b
x \\// y = rmap (either id id) $ x \/ y

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
