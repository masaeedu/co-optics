{-# LANGUAGE PatternSynonyms, ViewPatterns, LambdaCase #-}
module Data.FunList.Bazaar where

import MyPrelude

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Data.Profunctor

import Monoidal.Applicative

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy
import Profunctor.Kleisli

import Optics.Types
import Optics.Prism

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

fmap' :: (a -> b) -> Bazaar a x t -> Bazaar b x t
fmap' f (B b) = B $ b . (. f)

sequenceA' :: Applicative f => Bazaar (f a) x t -> f (Bazaar a x t)
sequenceA' (B b) = getCompose . b $ \fa -> Compose $ fmap singleton $ fa

traverse' :: Applicative f => (a -> f b) -> Bazaar a x t -> f (Bazaar b x t)
traverse' f = sequenceA' . fmap' f

foldMap' :: Monoid m => (a -> m) -> Bazaar a x t -> m
foldMap' f (B b) = getConst $ b $ Const . f

contents :: Bazaar a b t -> [a]
contents = foldMap' pure

-- aka sold
fuse :: Bazaar b b t -> t
fuse (B b) = runIdentity $ b pure

data Unfun a b t = Done t | More a (Bazaar a b (b -> t))
  deriving Functor

_More :: Prism (Unfun a b t) (Unfun a' b' t) (a × Bazaar a b (b -> t)) (a' × Bazaar a' b' (b' -> t))
_More = prism (uncurry More) (\case { More a as -> Right (a, as); Done t -> Left $ Done t })

instance Apply (Unfun a b)
  where
  zip (Done x)    fy          = (x,) <$> fy
  zip (More a fx) (inn -> fy) = More a $ fmap (\(x, y) b -> (x b, y)) $ zip fx fy

instance Applicative (Unfun a b)
  where
  pure = Done

out :: Bazaar a b t -> Unfun a b t
out (B b) = b (\a -> More a $ pure id)

inn :: Unfun a b t -> Bazaar a b t
inn (Done t)    = done t
inn (More a as) = more a as

unfun :: Iso (Bazaar a b t) (Bazaar a' b' t') (Unfun a b t) (Unfun a' b' t')
unfun = dimap out inn

traversed :: Traversal (Bazaar a x t) (Bazaar b x t) a b
traversed (pab :: p a b) = rec
  where
  rec :: p (Bazaar a x t) (Bazaar b x t)
  rec = unfun $ _More $ pab /\ defer (\_ -> rec)

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

{-# COMPLETE BTraversal #-}

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
traverseOf b = case t2fl b of (BTraversal b') -> b'
