module Data.FunList.FunList where

import MyPrelude

import Data.Profunctor

import Monoidal.Applicative

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy
import Profunctor.Kleisli

import Optics.Types

data FunList a b t = Done t | More a (FunList a b (b -> t))

instance Functor (FunList a b)
  where
  fmap f (Done x) = Done $ f x
  fmap f (More a b) = More a $ (f <<<) <$> b

instance Apply (FunList a b)
  where
  zip (Done x)    fy = (x,) <$> fy
  zip (More a fx) fy = More a $ fmap (\(x, y) b -> (x b, y)) $ zip fx fy

instance Applicative (FunList a b)
  where
  pure = Done

singleton :: x -> FunList x y y
singleton x = More x $ Done id

sequenceA' :: Applicative f => FunList (f a) b t -> f (FunList a b t)
sequenceA' (Done a) = pure $ Done a
sequenceA' (More a b) = More <$> a <*> sequenceA' b

foldMap' :: Monoid m => (a -> m) -> FunList a b t -> m
foldMap' _ (Done _) = mempty
foldMap' f (More a as) = f a <> foldMap' f as

contents :: FunList a b t -> [a]
contents = foldMap' pure

fuse :: FunList b b t -> t
fuse (Done t)    = t
fuse (More a as) = fuse as a

type Unfun a b t  = t + a × FunList a b (b -> t)

out :: FunList a b t -> Unfun a b t
out (Done t) = Left t
out (More a as) = Right $ (a, as)

inn :: Unfun a b t -> FunList a b t
inn (Left t) = Done t
inn (Right (a, as)) = More a as

unfun :: Iso (FunList a b t) (FunList a' b' t') (Unfun a b t) (Unfun a' b' t')
unfun = dimap out inn

traversed :: Traversal (FunList a x t) (FunList b x t) a b
traversed (pab :: p a b) = rec
  where
  rec :: p (FunList a x t) (FunList b x t)
  rec = unfun $ right' $ pab /\ defer (\_ -> rec)

newtype Traversal0 a b s t = T0 { runT0 :: s -> FunList a b t }
  deriving (Profunctor, Strong, Choice, Mux, Visitor, Demux, Switch) via (Kleisli (FunList a b))
  deriving Lazy via (Kleisli (FunList a b) s t)

fl2t :: Traversal0 a b s t -> Traversal s t a b
fl2t (T0 t) = dimap t fuse . traversed

t2fl :: Traversal s t a b -> Traversal0 a b s t
t2fl t = t $ T0 $ singleton

each' :: Traversable t => Traversal0 a b (t a) (t b)
each' = T0 $ traverse singleton

each :: Traversable t => Traversal (t a) (t b) a b
each = fl2t each'
