{-# LANGUAGE LambdaCase, EmptyCase, AllowAmbiguousTypes #-}
module SOP.Products where

import MyPrelude

import GHC.Generics

import Data.Void
import Data.Proxy

import Optics

type family TProduct (xs :: [*]) :: *
  where
  TProduct '[] = ()
  TProduct (x ': xs) = (x, TProduct xs)

class Append (xs :: [*]) (ys :: [*]) (zs :: [*]) | xs ys -> zs
  where
  (&&&) :: Proxy xs -> Proxy ys -> Iso' (TProduct xs, TProduct ys) (TProduct zs)

instance Append '[] ys ys
  where
  _ &&& _ = iso snd ((),)

instance Append xs ys zs => Append (x ': xs) ys (x ': zs)
  where
  (_ :: Proxy (x ': xs)) &&& (_ :: Proxy ys) = iso
    (\((x, xs), ys) -> (x, fwd (Proxy @xs &&& Proxy @ys) (xs, ys)))
    (\(x, zs) -> let (xs, ys) = bwd (Proxy @xs &&& Proxy @ys) zs in ((x, xs), ys))

class Product c xs | c -> xs
  where
  asProduct :: Iso' c (TProduct xs)

instance Product (V1 p) '[Void]
  where
  asProduct = iso (\case {}) (absurd . fst)

instance Product (U1 p) '[]
  where
  asProduct = iso (const ()) (const U1)

instance Product (K1 i c p) '[c]
  where
  asProduct = iso (\(K1 c) -> (c, ())) (\(c, _) -> K1 c)

instance Product (f p) xs => Product (M1 i c f p) xs
  where
  asProduct = iso (\(M1 fp) -> fwd asProduct fp) (M1 . bwd asProduct)

instance (Product (f p) xs, Product (g p) ys, Append xs ys zs) => Product ((f :*: g) p) zs
  where
  asProduct = iso
    (\(fp :*: gp) -> fwd (Proxy @xs &&& Proxy @ys) $ (fwd asProduct fp, fwd asProduct gp))
    ((\(xs, ys) -> bwd asProduct xs :*: bwd asProduct ys) . bwd (Proxy @xs &&& Proxy @ys))

gprod :: forall a x xs. (Generic a, Product (Rep a x) xs) => Iso' a (TProduct xs)
gprod = iso from to . asProduct @(Rep a x) @xs
