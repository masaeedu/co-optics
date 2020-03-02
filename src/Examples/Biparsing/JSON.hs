{-# LANGUAGE LambdaCase, DeriveGeneric #-}
module Examples.Biparsing.JSON where

import MyPrelude hiding (exponent)

import GHC.Generics
import GHC.Natural

import Data.Char (readLitChar)
import Data.Profunctor
import Data.List.NonEmpty (NonEmpty(..))

import Data.Generics.Wrapped (_Unwrapped)

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy

import Monoidal.Applicative

import Optics

import SOP.Sums
import SOP.Products

import Examples.Biparsing.Common

data JSON = JSON { lpad :: Whitespace, val :: Value, rpad :: Whitespace }
  deriving (Generic, Show)

json :: Biparser' Maybe JSON
json = gprod $ jsonWhitespace /\ jsonValue /\ jsonWhitespace /\ start

data Value = N Number | S String | B Bool | Null | O Object | A Array
  deriving (Generic, Show)

jsonValue :: Biparser' Maybe Value
jsonValue = gsum $ jsonNumber \/ jsonString \/ jsonBool \/ jsonNull \/ jsonObject \/ jsonArray \/ stop

data Number = Number { whole :: Int, fraction :: Maybe Natural, exponent :: Maybe Int }
  deriving (Generic, Show)

jsonNumber :: Biparser' Maybe Number
jsonNumber = gprod $ int /\ perhaps nat /\ perhaps int /\ start

data SpaceChar = Space | LF | CR | Tab
  deriving (Generic, Show)

type Whitespace = [SpaceChar]

jsonSC :: Biparser' Maybe SpaceChar
jsonSC = gsum $ token_ " " \/ token_ "\n" \/ token_ "\r" \/ token_ "\t" \/ stop

jsonWhitespace :: Biparser' Maybe Whitespace
jsonWhitespace = each jsonSC

data Escape = Escape { code :: Char }
  deriving (Generic, Show)

-- Special characters in a JSON string (which are escaped with backslashes)
jsonSChar :: Biparser' Maybe Escape
jsonSChar = token_ "\\" -\ (convert _Unwrapped . re (among "\"\\/bfnrt")) char

-- Normal characters  in a JSON string (no escaping required)
jsonNChar :: Biparser' Maybe Char
jsonNChar = re (predicate (\c -> c /= '\\' && c /= '"')) char

unescape :: Iso' Char (Escape + Char)
unescape = iso fwd bwd
  where
  fwd '"'  = Left $ Escape '"'
  fwd '\\' = Left $ Escape '\\'
  fwd '/'  = Left $ Escape '/'
  fwd '\b' = Left $ Escape 'b'
  fwd '\f' = Left $ Escape 'f'
  fwd '\n' = Left $ Escape 'n'
  fwd '\r' = Left $ Escape 'r'
  fwd '\t' = Left $ Escape 't'
  fwd x    = Right x

  bwd (Left (Escape '"')) = '"'
  bwd (Left (Escape '\\')) = '\\'
  bwd (Left (Escape '/')) = '/'
  bwd (Left (Escape 'b')) = '\b'
  bwd (Left (Escape 'f')) = '\f'
  bwd (Left (Escape 'n')) = '\n'
  bwd (Left (Escape 'r')) = '\r'
  bwd (Left (Escape 't')) = '\t'
  bwd (Left (Escape c))   = fst $ head $ readLitChar ['\\', c]
  bwd (Right c)           = c

jsonString :: Biparser' Maybe String
jsonString = token_ "\"" -\ (each . unescape) (jsonSChar \/ jsonNChar) /- token_ "\""

jsonBool :: Biparser' Maybe Bool
jsonBool = gsum $ token_ "true" \/ token_ "false" \/ stop

jsonNull :: Biparser' Maybe ()
jsonNull = token_ "null"

data Entry = Entry { lead :: Whitespace, key :: String, sep :: Whitespace, value :: JSON }
  deriving (Generic, Show)

jsonEntry :: Biparser' Maybe Entry
jsonEntry = gprod $ jsonWhitespace /\ jsonString /\ jsonWhitespace /\ token_ ":" -\ (defer $ \_ -> json) /\ start

type Object = NonEmpty Entry + Whitespace

jsonObject :: Biparser' Maybe Object
jsonObject = token_ "{" -\ (commaSeparated jsonEntry \/ jsonWhitespace) /- token_ "}"

type Array = NonEmpty JSON + Whitespace

jsonArray :: Biparser' Maybe Array
jsonArray = token_ "[" -\ (commaSeparated (defer $ \_ -> json) \/ jsonWhitespace) /- token_ "]"
