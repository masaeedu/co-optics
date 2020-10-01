{-# LANGUAGE ImpredicativeTypes #-}

module Optics.Types where

import Data.Functor.Monoidal.Applicative (pure)

import Data.Profunctor (Profunctor(..))
import Data.Profunctor.Strong.Class
import Data.Profunctor.Monoidal.Class
import Data.Profunctor.Lazy

import Data.Biprofunctor (Biprofunctor(..))
import Data.Biprofunctor.Upstream (Upstream(..))
import qualified Data.Biprofunctor.Monoidal.Class as BP

import Control.Category.Product (ProductCategory(..))

import qualified Control.Category.Tensor as T

newtype Optic p a b s t = Optic { runOptic :: p a b -> p s t }

type Iso       a b s t = forall p. Profunctor       p => Optic p a b s t
type Lens      a b s t = forall p. Strong   (×) (×) p => Optic p a b s t
type Colens    a b s t = forall p. OpStrong (×) (×) p => Optic p a b s t
type Prism     a b s t = forall p. Strong   (+) (+) p => Optic p a b s t
type Coprism   a b s t = forall p. OpStrong (+) (+) p => Optic p a b s t

type Traversing p = (Monoidal (×) (×) (×) p, Strong (+) (+) p, Lazy2 p)

type Traversal a b s t = forall p. Traversing       p => Optic p a b s t

type Optic'   p a s = Optic   p a a s s
type Iso'       a s = Iso       a a s s
type Lens'      a s = Lens      a a s s
type Colens'    a s = Colens    a a s s
type Prism'     a s = Prism     a a s s
type Coprism'   a s = Coprism   a a s s
type Traversal' a s = Traversal a a s s

-- Biprofunctor
instance Profunctor p => Biprofunctor (Optic p)
  where
  bidimap f g h i (Optic p) = Optic $ dimap h i . p . dimap f g

-- Monoidal biprofunctor
instance
  ( OpSemigroupal t1 t2 (×) p -- mathematically we can put an existential `t` instead of `(×)` here, but that causes problems with type application
  , Semigroupal   t3 t4 (×) p
  ) =>
  BP.Semigroupal t1 t2 t3 t4 (×) (Optic p)
  where
  combineBP (Optic p, Optic q) = Optic $ combineP . T.bimap p q . uncombineP @t1 @t2 @(×)

instance
  ( OpMonoidal t1 t2 (×) p
  , Monoidal   t3 t4 (×) p
  ) =>
  BP.Monoidal t1 t2 t3 t4 (×) (Optic p)
  where
  unitBP () = Optic $ unitP @t3 @t4 @(×) . discardP @t1 @t2 @(×)

(-+×××-) :: Mux p => Optic p a b s t -> Optic p a' b' s' t' -> Optic p (a + a') (b × b') (s × s') (t × t')
(-+×××-) = (BP.-?-)

(-+××+-) :: Switch p => Optic p a b s t -> Optic p a' b' s' t' -> Optic p (a + a') (b × b') (s × s') (t + t')
(-+××+-) = (BP.-?-)

-- ProductCategory
instance ProductCategory (Optic p)
  where
  biid = Optic id
  bicompose (Optic f) (Optic g) = Optic $ f . g

-- Profunctor
instance Profunctor p => Profunctor (Optic p a b)
  where
  dimap f g (Optic p) = Optic $ dimap f g . p

-- Strong, Costrong, Choice, Cochoice
instance LStrong t1 t2 p => LStrong t1 t2 (Optic p a b)
  where
  lstrength (Optic p) = Optic $ lstrength . p

instance RStrong t1 t2 p => RStrong t1 t2 (Optic p a b)
  where
  rstrength (Optic p) = Optic $ rstrength . p

instance OpLStrong t1 t2 p => OpLStrong t1 t2 (Optic p a b)
  where
  oplstrength (Optic p) = Optic $ oplstrength . p

instance OpRStrong t1 t2 p => OpRStrong t1 t2 (Optic p a b)
  where
  oprstrength (Optic p) = Optic $ oprstrength . p

instance LStrong t2 t1 p => OpLStrong t1 t2 (Upstream (Optic p) s t)
  where
  oplstrength (Upstream (Optic p)) = Upstream $ Optic $ p . lstrength

instance RStrong t2 t1 p => OpRStrong t1 t2 (Upstream (Optic p) s t)
  where
  oprstrength (Upstream (Optic p)) = Upstream $ Optic $ p . rstrength

instance OpLStrong t2 t1 p => LStrong t1 t2 (Upstream (Optic p) s t)
  where
  lstrength (Upstream (Optic p)) = Upstream $ Optic $ p . oplstrength

instance OpRStrong t2 t1 p => RStrong t1 t2 (Upstream (Optic p) s t)
  where
  rstrength (Upstream (Optic p)) = Upstream $ Optic $ p . oprstrength

-- Mux, Demux, Switch, Splice
instance Semigroupal t1 t2 o p => Semigroupal t1 t2 o (Optic p a b)
  where
  combineP = Optic . (\o pab -> combineP $ T.bimap ($ pab) ($ pab) o) . T.bimap runOptic runOptic

-- Terminal, Initial, Universal, Unique
instance Monoidal m1 m2 o p => Monoidal m1 m2 o (Optic p a b)
  where
  unitP = Optic . pure . unitP @m1 @m2 @(o)
