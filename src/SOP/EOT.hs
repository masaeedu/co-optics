{-# LANGUAGE LambdaCase, EmptyCase, DeriveAnyClass, DeriveGeneric #-}
module SOP.EOT where

import MyPrelude

import Data.Void
import Data.Bifunctor

import Generics.SOP

import Optics.Iso

type x + y = Either x y
infixr +

type x × y = (x, y)
infixr ×

class
  Product (xs :: [*]) (p :: *) | xs -> p
  where
  asProduct :: Iso' (NP I xs) p

instance
  Product '[] ()
  where
  asProduct = iso (const ()) (const Nil)

instance
  Product xs p
  => Product (x ': xs) (x × p)
  where
  asProduct = iso
    (\case { (fx :* xs) -> (unI fx, fwd asProduct xs) })
    (\(fx, p) -> I fx :* bwd asProduct p)

class
  SumOfProducts (xxs :: [[*]]) (pe :: *) | xxs -> pe
  where
  asSumOfProducts :: Iso' (SOP I xxs) pe

instance
  SumOfProducts '[] Void
  where
  asSumOfProducts = iso (\case {} . unSOP) absurd

instance
  ( Product x p
  , SumOfProducts xs pe
  )
  => SumOfProducts (x ': xs) (p + pe)
  where
  asSumOfProducts = iso
    (unSOP >>> \case { Z fx -> Left $ fwd asProduct fx; S xs -> Right $ fwd asSumOfProducts $ SOP $ xs })
    (SOP . either (Z . bwd asProduct) (S . unSOP . bwd asSumOfProducts))

data Cases = Recurse | Remove | Done

type family TEnd p :: Cases
  where
  TEnd (x × ()) = 'Remove
  TEnd (x × y ) = 'Recurse
  TEnd _        = 'Done

class
  TEnd p ~ c
  => TNormalize (c :: Cases) (p :: *) (n :: *) | c p -> n
  where
  tnormalize :: Iso' p n

instance
  TEnd x ~ 'Done
  => TNormalize 'Done x x
  where
  tnormalize = iso id id

instance
  TNormalize 'Remove (x × ()) x
  where
  tnormalize = iso fst (, ())

instance
  ( TEnd (x × y) ~ 'Recurse
  , TNormalize b y y'
  )
  => TNormalize 'Recurse (x × y) (x × y')
  where
  tnormalize = iso (second $ fwd $ tnormalize) (second $ bwd $ tnormalize)

type family EEnd e :: Cases
  where
  EEnd (x + Void) = 'Remove
  EEnd (x + y   ) = 'Recurse
  EEnd _          = 'Done

class
  EEnd e ~ c
  => ENormalize (c :: Cases) (e :: *) (n :: *) | c e -> n
  where
  enormalize :: Iso' e n

instance
  ( EEnd x ~ 'Done
  , TNormalize b x x'
  )
  => ENormalize 'Done x x'
  where
  enormalize = tnormalize

instance
  TNormalize a x x'
  => ENormalize 'Remove (x + Void) x'
  where
  enormalize = iso (either (fwd tnormalize) absurd) (Left . bwd tnormalize)

instance
  ( EEnd (x + y) ~ 'Recurse
  , TNormalize a x x'
  , ENormalize b y y'
  )
  => ENormalize 'Recurse (x + y) (x' + y')
  where
  enormalize = iso
    (bimap (fwd $ tnormalize) (fwd $ enormalize))
    (bimap (bwd $ tnormalize) (bwd $ enormalize))

gsop ::
  ( Generic x
  , Code x ~ xxs
  , SumOfProducts xxs pe
  , EEnd pe ~ b
  , ENormalize b pe pe'
  )
  => Iso' x pe'
gsop = iso
  (\x -> fwd enormalize $ fwd asSumOfProducts $ from $ x)
  (\n -> to $ bwd asSumOfProducts $ bwd enormalize $ n)
