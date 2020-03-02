{-# LANGUAGE TypeFamilies, FunctionalDependencies, LambdaCase, AllowAmbiguousTypes #-}
module SOP.Sums where

import MyPrelude

import GHC.Generics

import Data.Proxy
import Data.Void
import Data.Bifunctor (Bifunctor(..))

import Optics

type family Sum (xs :: [*]) :: *
  where
  Sum '[] = Void
  Sum (x ': xs) = Either x (Sum xs)

class Coproduct c xs | c -> xs
  where
  asCoproduct :: Iso' c (Sum xs)

instance Coproduct (U1 p) '[()]
  where
  asCoproduct = iso (\U1 -> Left ()) (either (const U1) absurd)

instance Coproduct (K1 i c p) '[c]
  where
  asCoproduct = iso (\(K1 c) -> Left c) (either K1 absurd)

instance (Functor f, Coproduct (f p) xs) => Coproduct (M1 i c f p) xs
  where
  asCoproduct = iso (\(M1 fp) -> fwd asCoproduct fp) (M1 . bwd asCoproduct)

class Append (xs :: [*]) (ys :: [*]) (zs :: [*]) | xs ys -> zs
  where
  (|||) :: Proxy xs -> Proxy ys -> Iso' (Either (Sum xs) (Sum ys)) (Sum zs)

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

instance (Coproduct (f p) xs, Coproduct (g p) ys, Append xs ys zs) => Coproduct ((f :+: g) p) zs
  where
  asCoproduct = iso
    (\case
      { L1 fp -> fwd (Proxy @xs ||| Proxy @ys) $ Left  $ fwd asCoproduct fp
      ; R1 gp -> fwd (Proxy @xs ||| Proxy @ys) $ Right $ fwd asCoproduct gp
      })
    (either (L1 . bwd asCoproduct) (R1 . bwd asCoproduct) . bwd (Proxy @xs ||| Proxy @ys))

-- TODO: @lysxia suggested that this can be done better using a CPS approach here https://funprog.zulipchat.com/#narrow/stream/201385-Haskell/topic/Illegal.20type.20synonym.20family.20in.20instance/near/189455360
-- try that out

gcoprod :: forall a x xs. (Generic a, Coproduct (Rep a x) xs) => Iso' a (Sum xs)
gcoprod = iso from to . asCoproduct @(Rep a x) @xs
