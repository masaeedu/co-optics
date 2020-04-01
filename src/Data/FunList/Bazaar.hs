{-# LANGUAGE PartialTypeSignatures, PatternSynonyms, ViewPatterns #-}
module Data.FunList.Bazaar where

import MyPrelude

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Data.Profunctor

import qualified Data.FunList.FunList as F

import Monoidal.Applicative

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy
import Profunctor.Kleisli

import Optics.Types

newtype Bazaar a b t = B { runB :: forall f. Applicative f => (a -> f b) -> f t }
  deriving Functor

instance Apply (Bazaar a b)
  where
  zip (B x) (B y) = B $ \v -> zip (x v) (y v)

instance Applicative (Bazaar a b)
  where
  pure a = B $ const $ pure a

done :: t -> Bazaar a b t
done = pure

more :: a -> Bazaar a b (b -> t) -> Bazaar a b t
more a (B b) = B $ \f -> flip ($) <$> f a <*> b f

-- aka sell
singleton :: x -> Bazaar x y y
singleton x = B $ \f -> f x

sequenceA' :: Applicative f => Bazaar (f a) x t -> f (Bazaar a x t)
sequenceA' (B b) = getCompose . b $ \x -> Compose $ fmap singleton $ x

foldMap' :: Monoid m => (a -> m) -> Bazaar a x t -> m
foldMap' f (B b) = getConst $ b $ Const . f

contents :: Bazaar a b t -> [a]
contents = foldMap' pure

-- aka sold
fuse :: Bazaar b b t -> t
fuse (B b) = runIdentity $ b pure

type Unfun a b t = t + a × Bazaar a b (b -> t)

-- Now we can choose any concrete representation for the free applicative generated by the indexed Store comonad

-- One representation is `FunList a b t`
fwd :: F.FunList a b t -> Bazaar a b t
fwd (F.Done t)   = B $ \_ -> pure t
fwd (F.More a r) = B $ \f -> flip ($) <$> f a <*> runB (fwd r) f

bwd :: Bazaar a b t -> F.FunList a b t
bwd (B b) = b $ F.singleton

out :: Bazaar a b t -> Unfun a b t
out = (fmap $ fmap $ fwd) . F.out . bwd

{-
TODO:
1. Add (FreeApplicative Store) representation
2. Add (exists n. (Vec n a, Vec n b -> t)) representation
-}

inn :: Unfun a b t -> Bazaar a b t
inn (Left t) = done t
inn (Right (a, as)) = more a as

unfun :: Iso (Bazaar a b t) (Bazaar a' b' t') (Unfun a b t) (Unfun a' b' t')
unfun = dimap out inn

traversed :: Traversal (Bazaar a x t) (Bazaar b x t) a b
traversed (pab :: p a b) = rec
  where
  rec :: p (Bazaar a x t) (Bazaar b x t)
  rec = unfun $ right' $ pab /\ defer (\_ -> rec)

newtype Traversal1 a b s t = T1 { runT1 :: s -> Bazaar a b t }
  deriving (Profunctor, Strong, Choice, Mux, Visitor, Demux, Switch) via (Kleisli (Bazaar a b))
  deriving Lazy via (Kleisli (Bazaar a b) s t)

construct :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> Traversal1 a b s t
construct b = T1 $ \s -> B $ flip b $ s

destruct :: Traversal1 a b s t -> (forall f. Applicative f => (a -> f b) -> s -> f t)
destruct (T1 x) afb s = runB (x s) afb

pattern BTraversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> Traversal1 a b s t
pattern BTraversal b <- (destruct -> b)
  where
  BTraversal b = construct b

fl2t :: Traversal1 a b s t -> Traversal s t a b
fl2t (T1 t) = dimap t fuse . traversed

t2fl :: Traversal s t a b -> Traversal1 a b s t
t2fl t = t $ T1 $ singleton

each' :: Traversable t => Traversal1 a b (t a) (t b)
each' = T1 $ traverse singleton

each :: Traversable t => Traversal (t a) (t b) a b
each = fl2t each'

wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
wander b = fl2t $ BTraversal b

traverseOf :: Traversal s t a b -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf b = case t2fl b of
  (BTraversal b) -> b
