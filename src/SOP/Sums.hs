{-# LANGUAGE LambdaCase, EmptyCase, AllowAmbiguousTypes #-}
module SOP.Sums (Coproduct(..), ESum, gsum) where

import MyPrelude

import GHC.Generics

import Data.Proxy
import Data.Void
import Data.Bifunctor (Bifunctor(..))

import Optics

type family ESum (xs :: [*]) :: *
  where
  ESum '[] = Void
  ESum (x ': xs) = Either x (ESum xs)

class Append (xs :: [*]) (ys :: [*]) (zs :: [*]) | xs ys -> zs
  where
  (|||) :: Proxy xs -> Proxy ys -> Iso' (Either (ESum xs) (ESum ys)) (ESum zs)

instance Append '[] ys ys
  where
  _ ||| _ = iso (either absurd id) Right

instance Append xs ys zs => Append (x ': xs) ys (x ': zs)
  where
  (_ :: Proxy (x ': xs)) ||| (_ :: Proxy ys) = iso
    (either
      (either Left (Right . fwd (Proxy @xs ||| Proxy @ys) . Left))
      (Right . fwd (Proxy @xs ||| Proxy @ys) . Right))
    (either (Left . Left) (first Right . bwd (Proxy @xs ||| Proxy @ys)))

class Coproduct c xs | c -> xs
  where
  asSum :: Iso' c (ESum xs)

instance Coproduct (V1 p) '[]
  where
  asSum = iso (\case {}) absurd

instance Coproduct (U1 p) '[()]
  where
  asSum = iso (\U1 -> Left ()) (either (const U1) absurd)

instance Coproduct (K1 i c p) '[c]
  where
  asSum = iso (\(K1 c) -> Left c) (either K1 absurd)

instance Coproduct (f p) xs => Coproduct (M1 i c f p) xs
  where
  asSum = iso (\(M1 fp) -> fwd asSum fp) (M1 . bwd asSum)

instance (Coproduct (f p) xs, Coproduct (g p) ys, Append xs ys zs) => Coproduct ((f :+: g) p) zs
  where
  asSum = iso
    (\case
      { L1 fp -> fwd (Proxy @xs ||| Proxy @ys) $ Left  $ fwd asSum fp
      ; R1 gp -> fwd (Proxy @xs ||| Proxy @ys) $ Right $ fwd asSum gp
      })
    (either (L1 . bwd asSum) (R1 . bwd asSum) . bwd (Proxy @xs ||| Proxy @ys))

-- TODO: @lysxia suggested that this can be done better using a CPS approach here https://funprog.zulipchat.com/#narrow/stream/201385-Haskell/topic/Illegal.20type.20synonym.20family.20in.20instance/near/189455360
-- try that out

gsum :: forall a x xs. (Generic a, Coproduct (Rep a x) xs) => Iso' a (ESum xs)
gsum = iso from to . asSum @(Rep a x) @xs
