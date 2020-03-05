{-# LANGUAGE LambdaCase, EmptyCase, DeriveAnyClass, DeriveGeneric #-}
module SOP.EOT where

import MyPrelude

import Data.Void
import Data.Bifunctor

import Generics.SOP

import Optics.Types
import Optics.Iso.Combinators

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

type family TCase p :: Cases
  where
  TCase (x × ()) = 'Remove
  TCase (x × y ) = 'Recurse
  TCase _        = 'Done

class
  TCase p ~ c
  => TNormalize (c :: Cases) (p :: *) (n :: *) | c p -> n
  where
  tnormalize :: Iso' p n

instance
  TCase x ~ 'Done
  => TNormalize 'Done x x
  where
  tnormalize = iso id id

instance
  TNormalize 'Remove (x × ()) x
  where
  tnormalize = iso fst (, ())

instance
  ( TCase (x × y) ~ 'Recurse
  , TNormalize b y y'
  )
  => TNormalize 'Recurse (x × y) (x × y')
  where
  tnormalize = iso (second $ fwd $ tnormalize) (second $ bwd $ tnormalize)

type family ECase e :: Cases
  where
  ECase (x + Void) = 'Remove
  ECase (x + y   ) = 'Recurse
  ECase _          = 'Done

class
  ECase e ~ c
  => ENormalize (c :: Cases) (e :: *) (n :: *) | c e -> n
  where
  enormalize :: Iso' e n

instance
  ( ECase x ~ 'Done
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
  ( ECase (x + y) ~ 'Recurse
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
  , Generic y
  , Code x ~ xxs
  , Code y ~ yys
  , SumOfProducts xxs pe1
  , SumOfProducts yys pe2
  , ECase pe1 ~ b1
  , ECase pe2 ~ b2
  , ENormalize b1 pe1 pe1'
  , ENormalize b2 pe2 pe2'
  )
  => Iso x y pe1' pe2'
gsop = iso
  (\x -> fwd enormalize $ fwd asSumOfProducts $ from $ x)
  (\n -> to $ bwd asSumOfProducts $ bwd enormalize $ n)
