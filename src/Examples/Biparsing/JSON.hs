{-# LANGUAGE LambdaCase, DeriveGeneric #-}
module Examples.Biparsing.JSON where

import MyPrelude hiding (exponent)

import GHC.Generics
import GHC.Natural

import Data.List.NonEmpty (NonEmpty(..))

import Data.Generics.Wrapped (_Wrapped, _Unwrapped)

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy

import Optics

import SOP.Sums
import SOP.Products

import Examples.Biparsing.Common

-- Let's build a biparser for the JSON grammar described here: https://www.json.org/json-en.html

data JSON = JSON { lpad :: Whitespace, val :: Value, rpad :: Whitespace }
  deriving (Generic, Show)

-- Parse/print a JSON document
json :: Biparser' Maybe JSON
json = gprod $
  jsonWhitespace /\
  jsonValue      /\
  jsonWhitespace /\ start

-- Parse/print a character representing a space in a JSON document
jsonSpaceChar :: Biparser' Maybe SpaceChar
jsonSpaceChar = gsum $
  token_ " "  \/
  token_ "\n" \/
  token_ "\r" \/
  token_ "\t" \/ stop

type Whitespace = [SpaceChar]

-- Parse/print some whitespace in a JSON document
jsonWhitespace :: Biparser' Maybe Whitespace
jsonWhitespace = each jsonSpaceChar

data Value = N Number | S String | B Bool | Null | O Object | A Array
  deriving (Generic, Show)

-- Parse/print a JSON value (without leading/trailing whitespace)
jsonValue :: Biparser' Maybe Value
jsonValue = gsum $
  jsonNumber \/
  jsonString \/
  jsonBool   \/
  jsonNull   \/
  jsonObject \/
  jsonArray  \/ stop

data Number = Number { whole :: Int, fraction :: Maybe Natural, exponent :: Maybe Int }
  deriving (Generic, Show)

-- Parse/print a JSON number
jsonNumber :: Biparser' Maybe Number
jsonNumber = gprod $
  int                         /\
  perhaps (token_ "." \\ nat) /\
  perhaps (token_ "E" \\ int) /\ start

data SpaceChar = Space | LF | CR | Tab
  deriving (Generic, Show)

-- Parse/print a normal character in a JSON string (anything but quotes or backslashes)
jsonNChar :: Biparser' Maybe Char
jsonNChar = re (predicate (\c -> c /= '\\' && c /= '"')) char

-- Special characters which need to be escaped with backslashes
isSpecial :: Char -> Bool
isSpecial c = c `elem` "\"\\/\b\f\n\r\t"

-- Distinguish characters by whether they're special or not
specialVsNormal :: Iso' Char (Escape + Char)
specialVsNormal = distinguish isSpecial . liftIsoFirst (convert _Wrapped)

-- Parse/print the escape code of a special character
jsonEscapeCode :: Biparser' Maybe Char
jsonEscapeCode = re (predicate isSpecial) $ asEscapeCode $ convert _Unwrapped $ char

-- Parse/print an escaped special character in a JSON string
jsonSChar :: Biparser' Maybe Escape
jsonSChar =
  token_ "\\"
  \\
    convert _Unwrapped jsonEscapeCode

-- Parse/print a JSON string
jsonString :: Biparser' Maybe String
jsonString =
  token_ "\""
  \\
    (each $ specialVsNormal $ jsonSChar \/ jsonNChar)
  //
  token_ "\""

-- Parse/print a JSON boolean
jsonBool :: Biparser' Maybe Bool
jsonBool = gsum $
  token_ "true"  \/
  token_ "false" \/ stop

-- Parse/print a JSON null
jsonNull :: Biparser' Maybe ()
jsonNull = token_ "null"

data KeyValuePair = KeyValuePair { lead :: Whitespace, key :: String, sep :: Whitespace, value :: JSON }
  deriving (Generic, Show)

-- Parse/print a JSON key value pair
jsonKeyValuePair :: Biparser' Maybe KeyValuePair
jsonKeyValuePair = gprod $
  jsonWhitespace /\
  jsonString     /\
  jsonWhitespace /\
  token_ ":"
  \\
    (defer $ \_ -> json) /\ start

type Object = NonEmpty KeyValuePair + Whitespace

-- Parse/print a JSON object
jsonObject :: Biparser' Maybe Object
jsonObject =
  token_ "{"
  \\
    token_ "," `separated` jsonKeyValuePair \/ jsonWhitespace
  //
  token_ "}"

type Array = NonEmpty JSON + Whitespace

-- Parse/print a JSON array
jsonArray :: Biparser' Maybe Array
jsonArray =
  token_ "["
  \\
    token_ "," `separated` (defer $ \_ -> json) \/ jsonWhitespace
  //
  token_ "]"
