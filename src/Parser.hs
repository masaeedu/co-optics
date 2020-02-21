module Parser where

import MyPrelude

import Data.Bifunctor.Joker
import Data.Bifunctor.Product (Product(..))

import Control.Monad.State.Lazy (StateT(..))
import Control.Monad.Writer.Lazy (WriterT(..))

import Profunctor.Kleisli

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
