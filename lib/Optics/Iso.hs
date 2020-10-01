{-# LANGUAGE LambdaCase #-}
module Optics.Iso (module Optics.Iso, module Optics.Iso.Combinators) where

import Data.Void
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..))

import Optics.Types
import Optics.Reverse
import Optics.Iso.Combinators

import SOP.EOT

assocE :: Iso (a + (b + c)) (d + (e + f)) ((a + b) + c) ((d + e) + f)
assocE = iso
  (either (Left . Left) (either (Left . Right) Right))
  (either (either Left (Right . Left)) (Right . Right))

symmE :: Iso (a + b) (c + d) (b + a) (d + c)
symmE = iso
  (either Right Left)
  (either Right Left)

runitE :: Iso (a + Void) (b + Void) a b
runitE = iso (either id absurd) Left

lunitE :: Iso (Void + a) (Void + b) a b
lunitE = symmE . runitE

assocT :: Iso (a × (b × c)) (d × (e × f)) ((a × b) × c) ((d × e) × f)
assocT = iso
  (\(a, (b, c)) -> ((a, b), c))
  (\((a, b), c) -> (a, (b, c)))

symmT :: Iso (a × b) (c × d) (b × a) (d × c)
symmT = iso
  (\(a, b) -> (b, a))
  (\(a, b) -> (b, a))

runitT :: Iso (a × ()) (b × ()) a b
runitT = iso fst (, ())

lunitT :: Iso (() × a) (() × b) a b
lunitT = symmT . runitT

ldistribT :: Iso (a × (b + c)) (d × (e + f)) (a × b + a × c) (d × e + d × f)
ldistribT = iso
  (\(a, bc) -> bimap (a, ) (a, ) bc)
  (either (second Left) (second Right))

rdistribT :: Iso ((b + c) × a) ((e + f) × d) (b × a + c × a) (e × d + f × d)
rdistribT = symmT . ldistribT . bimapIso symmT symmT

uncons :: Iso [a] [b] (Maybe (a, [a])) (Maybe (b, [b]))
uncons = iso (\case { [] -> Nothing; (x : xs) -> Just (x, xs) }) (maybe [] (uncurry (:)))

maybeToEither :: Iso (Maybe a) (Maybe b) (() + a) (() + b)
maybeToEither = iso (maybe (Left ()) Right) (either (const Nothing) Just)

listToNonEmpty :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
listToNonEmpty = gsop . re maybeToEither . mapIso (re gsop)

unconsNonEmpty :: Iso (NonEmpty a) (NonEmpty b) (a × NonEmpty a + a) (b × NonEmpty b + b)
unconsNonEmpty =   --    NonEmpty a
  gsop             -- -> a × [a]
  . mapIso             --    [a]
    ( listToNonEmpty   -- -> Maybe (NonEmpty a)
    . maybeToEither    -- -> () + NonEmpty a
    . symmE            -- -> NonEmpty a + ()
    )             -- -> a × (NonEmpty a + ())
  . ldistribT     -- -> (a × NonEmpty a) + (a × ())
  . mapIso runitT -- -> (a × NonEmpty a) + a
