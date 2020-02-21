module Parser where

import MyPrelude

import Data.Bifunctor.Joker
import Data.Bifunctor.Product (Product(..))

import Control.Monad.Except (MonadError(..))

import Control.Monad.State.Lazy (StateT(..), get, put)
import Control.Monad.Writer.Lazy (WriterT(..), tell)

import Data.Digit (DecDigit, charDecimal)

import Profunctor.Kleisli
import Monoidal.Applicative
import Optics

type Parser  = Joker (StateT String Maybe)
type Printer = Kleisli (WriterT String Maybe)
type Biparser = Product Parser Printer

biparser :: StateT String Maybe o -> (i -> WriterT String Maybe o) -> Biparser i o
biparser x y = Pair (Joker x) (Kleisli y)

biparse :: Biparser i o -> String -> Maybe (o, String)
biparse (Pair (Joker (StateT x)) _) = x

biprint :: Biparser i o -> i -> Maybe (o, String)
biprint (Pair _ (Kleisli f)) = runWriterT . f

char :: Biparser Char Char
char = biparser r w
  where
  r = do
    s <- get
    case s of
      [] -> throwError ()
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
