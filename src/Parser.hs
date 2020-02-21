module Parser where

import MyPrelude

import Debug.Trace

import Data.Bifunctor.Joker
import Data.Bifunctor.Product (Product(..))

import Control.Monad.Except (MonadError(..))

import Control.Monad.State.Lazy (StateT(..), get, put)
import Control.Monad.Writer.Lazy (WriterT(..), tell)

import Data.Digit (DecDigit, charDecimal)

import Profunctor.Kleisli
import Monoidal.Applicative
import Optics

type F = Maybe
type Parser  = Joker (StateT String F)
type Printer = Kleisli (WriterT String F)
type Biparser = Product Parser Printer

biparser :: StateT String F o -> (i -> WriterT String F o) -> Biparser i o
biparser x y = Pair (Joker x) (Kleisli y)

parser :: Biparser i o -> Parser i o
parser (Pair p _) = p

runParser :: Parser i o -> String -> F (o, String)
runParser = runStateT . runJoker

printer :: Biparser i o -> Printer i o
printer (Pair _ p) = p

runPrinter :: Printer i o -> i -> F (o, String)
runPrinter = (runWriterT .) . runKleisli

biparse :: Biparser i o -> String -> F (o, String)
biparse = runParser . parser

biprint :: Biparser i o -> i -> F (o, String)
biprint = runPrinter . printer

char :: Biparser Char Char
char = biparser r w
  where
  r = do
    s <- get
    case s of
      [] -> StateT $ const Nothing
      (c : s') -> do
        put s'
        pure $ c

  w c = do
    tell [c]
    pure $ c

c2d :: Prism' Char DecDigit
c2d = convert charDecimal

digit :: Biparser DecDigit DecDigit
digit = re c2d char
