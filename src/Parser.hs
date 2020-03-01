{-# LANGUAGE ImpredicativeTypes #-}
module Parser where

import MyPrelude

import GHC.Natural

import Data.Digit (DecDigit(..))

import Data.Profunctor

import Data.Bifunctor.Joker
import Data.Bifunctor.Product (Product(..))

import Control.Monad.State.Lazy (StateT(..))
import Control.Monad.Writer.Lazy (WriterT(..))

import Profunctor.Kleisli
import Profunctor.Muxable
import Profunctor.Product

import Monoidal.Filterable
import Monoidal.Applicative
import Monoidal.Alternative

import Optics

type Parser   f = Joker (StateT String f)
type Printer  f = Kleisli (WriterT String f)
type Biparser f = Product (Parser f) (Printer f)
type Biparser' f x = Biparser f x x

biparser :: StateT String f o -> (i -> WriterT String f o) -> Biparser f i o
biparser x y = Pair (Joker x) (Kleisli y)

runParser :: Parser f i o -> String -> f (o, String)
runParser = runStateT . runJoker

runPrinter :: Printer f i o -> i -> f (o, String)
runPrinter = (runWriterT .) . runKleisli

biparse :: Biparser f i o -> String -> f (o, String)
biparse = runParser . pfst

biprint :: Biparser f i o -> i -> f (o, String)
biprint = runPrinter . psnd

type StaticParse f = (Applicative f, Alternative f, Filterable f)
type DynamicParse f = (StaticParse f, Monad f)

-- A biparser that parses one character
char :: StaticParse f => Biparser f Char Char
char = biparser r w
  where
  r = StateT $ \case { [] -> empty; (c : xs) -> pure (c, xs) }
  w c = WriterT $ pure (c, [c])

-- We can use a prism that focuses on whether a digit is a char as a coprism
-- to convert this into a biparser that produces a digit (or fails)
digit :: StaticParse f => Biparser' f DecDigit
digit = re c2d $ char

-- We can use a traversal to get a sequence of many digits
digits :: DynamicParse f => Biparser' f [DecDigit]
digits = each digit

-- We can use more coprisms to get turn a list of digits into a natural numbers
nat :: DynamicParse f => Biparser' f Natural
nat = re (asNonEmpty . digits2int) $ digits

-- We can use a coprism to get a prism focusing onto whether a character is a space
space :: StaticParse f => Biparser' f Char
space = re char2space char
  where
  char2space :: Prism' Char Char
  char2space = prism id (\case { ' ' -> Right ' '; c -> Left c })

-- We can make a combinator for repeating a biparser a given number of times
exactly :: DynamicParse f => Natural -> Biparser' f x -> Biparser' f [x]
exactly n p = bundle $ replicate (naturalToInt n) p

-- Ok, now we can solve the problem from the paper, which can be stated as follows:

-- > Parse a string that starts with a natural number, followed by a space, followed
-- > by the same number of characters as the natural number

-- Here's how we do it
lstring :: DynamicParse f => Biparser' f String
lstring = do
  n  <- nat   & lmap (intToNatural . length)
  _  <- space & lmap (const ' ')
  cs <- char  & replicate (naturalToInt n) & bundle
  pure cs
