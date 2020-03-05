{-# LANGUAGE LambdaCase, DeriveGeneric #-}
module Examples.Biparsing.JSON where

import MyPrelude hiding (exponent)

import GHC.Generics
import GHC.Natural

import Data.List.NonEmpty (NonEmpty(..))

import qualified Generics.SOP as SOP
import Data.Generics.Wrapped (_Wrapped, _Unwrapped)

import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Lazy

import Optics

import SOP.EOT

import Examples.Biparsing.Common

-- Let's build a biparser for the JSON grammar described here: https://www.json.org/json-en.html

data JSON = JSON { lpad :: Whitespace, val :: Value, rpad :: Whitespace }
  deriving (Generic, Show)
instance SOP.Generic JSON

-- Parse/print a JSON document
json :: Biparser' Maybe JSON
json = gsop $
  jsonWhitespace /\
  jsonValue      /\
  jsonWhitespace

data SpaceChar = Space | LF | CR | Tab
  deriving (Generic, Show)
instance SOP.Generic SpaceChar

-- Parse/print a character representing a space in a JSON document
jsonSpaceChar :: Biparser' Maybe SpaceChar
jsonSpaceChar = gsop $
  token_ " "  \/
  token_ "\n" \/
  token_ "\r" \/
  token_ "\t"

type Whitespace = [SpaceChar]

-- Parse/print some whitespace in a JSON document
jsonWhitespace :: Biparser' Maybe Whitespace
jsonWhitespace = many jsonSpaceChar

data Value = N Number | S String | B Bool | Null | O Object | A Array
  deriving (Generic, Show)
instance SOP.Generic Value

-- Parse/print a JSON value (without leading/trailing whitespace)
jsonValue :: Biparser' Maybe Value
jsonValue = gsop $
  jsonNumber \/
  jsonString \/
  jsonBool   \/
  jsonNull   \/
  jsonObject \/
  jsonArray

data Number = Number { whole :: Int, fraction :: Maybe Natural, exponent :: Maybe Int }
  deriving (Generic, Show)
instance SOP.Generic Number

-- Parse/print a JSON number
jsonNumber :: Biparser' Maybe Number
jsonNumber = gsop $
  int                         /\
  perhaps (token_ "." \\ nat) /\
  perhaps (token_ "E" \\ int)

-- Parse/print a normal character in a JSON string (anything but quotes or backslashes)
jsonNChar :: Biparser' Maybe Char
jsonNChar = re (predicate (\c -> c /= '\\' && c /= '"')) anychar

-- Special characters which need to be escaped with backslashes
isSpecial :: Char -> Bool
isSpecial c = c `elem` "\"\\/\b\f\n\r\t"

-- Distinguish characters by whether they're special or not
specialVsNormal :: Iso' Char (Escape + Char)
specialVsNormal = distinguish isSpecial . firstIso (convert _Wrapped)

-- Parse/print the escape code of a special character
jsonEscapeCode :: Biparser' Maybe Char
jsonEscapeCode = re (predicate isSpecial) $ asEscapeCode $ convert _Unwrapped $ anychar

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
    (many $ specialVsNormal $ jsonSChar \/ jsonNChar)
  //
  token_ "\""

-- Parse/print a JSON boolean
jsonBool :: Biparser' Maybe Bool
jsonBool = gsop $
  token_ "true"  \/
  token_ "false"

-- Parse/print a JSON null
jsonNull :: Biparser' Maybe ()
jsonNull = token_ "null"

data KeyValuePair = KeyValuePair { lead :: Whitespace, key :: String, sep :: Whitespace, value :: JSON }
  deriving (Generic, Show)
instance SOP.Generic KeyValuePair

-- Parse/print a JSON key value pair
jsonKeyValuePair :: Biparser' Maybe KeyValuePair
jsonKeyValuePair = gsop $
  jsonWhitespace /\
  jsonString     /\
  jsonWhitespace /\
  token_ ":"
  \\
    (defer $ \_ -> json)

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
