{-# LANGUAGE DeriveFunctor, DeriveGeneric, LambdaCase #-}
module Examples.Biparsing.DXF where

import GHC.Generics

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)

import Data.Digit (DecDigit(..))

import qualified Generics.SOP as SOP

import Profunctor.Mux
import Profunctor.Demux

import Optics
import SOP.EOT (gsop)

import Examples.Biparsing.Common

corpus :: [String]
corpus =
  [ "  0"
  , "SECTION"
  , "  2"
  , "HEADER"
  , "  9"
  , "$ACADVER"
  , "  1"
  , "AC1027"
  , "  9"
  , "$ACADMAINTVER"
  , " 70"
  , "     8"
  , "  9"
  , "$DWGCODEPAGE"
  , "  3"
  , "ANSI_1252"
  , "  9"
  , "$REQUIREDVERSIONS"
  , "160"
  , "                 0"
  , "  9"
  , "$LASTSAVEDBY"
  , "  1"
  , "williamrusnack"
  , "  9"
  , "$INSBASE"
  , " 10"
  , "0.0"
  , " 20"
  , "0.0"
  , " 30"
  , "0.0"
  , "  9"
  , "$EXTMIN"
  , " 10"
  , "4.910533931955369"
  , " 20"
  , "2.859738596236992"
  , " 30"
  , "0.0"
  ]

type ThreeDigitNumber =
    DecDigit × DecDigit × DecDigit
  + DecDigit × DecDigit
  + DecDigit

linenumber :: Biparser' Maybe ThreeDigitNumber
linenumber =
  (               digit /\ digit /\ digit) \/ -- Three digits, or
  (token_ " "  \\          digit /\ digit) \/ -- ... a space and two digits, or
  (token_ "  " \\                   digit)    -- ... two spaces and a digit

data EOL = LF | CRLF
  deriving (Generic, Show)

data DXFHeader = DXFHeader { sep :: EOL, lines :: NonEmpty (ThreeDigitNumber × String) }
  deriving (Generic, Show)

instance SOP.Generic DXFHeader

lheader :: Biparser' Maybe () -> Biparser' Maybe (NonEmpty (ThreeDigitNumber × String))
lheader eol = eol `separated` (linenumber /\ eol \\ many (besides "\r\n"))

header :: Biparser' Maybe DXFHeader
header = gsop $ massage $ lheader (token_ "\r\n") \/ lheader (token_ "\n")
  where
  massage = iso
    (\case { (CRLF, x) -> Left x; (LF, x) -> Right x })
    (either (CRLF,) (LF,))

testDXF :: IO ()
testDXF = do
  let
    test text = fmap (== text) $ biparse_ header text >>= biprint_ header
  print $ test $ intercalate "\n" corpus -- Just True
  print $ test $ intercalate "\r\n" corpus -- Just True
