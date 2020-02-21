module Parser where

import MyPrelude

import GHC.Natural

import Data.Bifunctor.Joker
import Data.Bifunctor.Product (Product(..))

import Data.List.NonEmpty

import Control.Monad.State.Lazy (StateT(..), get, put)
import Control.Monad.Writer.Lazy (WriterT(..), tell)

import Data.Digit (DecDigit, charDecimal)

import Profunctor.Kleisli
import Monoidal.Applicative
import Monoidal.Alternative
import Optics

type Parser   f = Joker (StateT String f)
type Printer  f = Kleisli (WriterT String f)
type Biparser f = Product (Parser f) (Printer f)

biparser :: StateT String f o -> (i -> WriterT String f o) -> Biparser f i o
biparser x y = Pair (Joker x) (Kleisli y)

parser :: Biparser f i o -> Parser f i o
parser (Pair p _) = p

runParser :: Parser f i o -> String -> f (o, String)
runParser = runStateT . runJoker

printer :: Biparser f i o -> Printer f i o
printer (Pair _ p) = p

runPrinter :: Printer f i o -> i -> f (o, String)
runPrinter = (runWriterT .) . runKleisli

biparse :: Biparser f i o -> String -> f (o, String)
biparse = runParser . parser

biprint :: Biparser f i o -> i -> f (o, String)
biprint = runPrinter . printer

-- A biparser
char :: Biparser Maybe Char Char
char = biparser r w
  where
  r = do
    s <- get
    case s of
      [] -> StateT $ const empty
      (c : s') -> do
        put s'
        pure $ c

  w c = do
    tell [c]
    pure $ c

-- Same biparser run through a backwards prism
digit :: Biparser Maybe DecDigit DecDigit
digit = re c2d char

int :: Biparser Maybe Natural Natural
int = re (asNonEmpty . digits2int) $ each $ digit
