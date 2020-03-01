{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Examples.Biparsing.Common where

import MyPrelude

import GHC.Natural

import Data.Profunctor

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Digit

import Control.Monad.State (StateT(..), MonadState(..))
import Control.Monad.Writer (WriterT(..), MonadWriter(..))

import Profunctor.Joker
import Profunctor.Kleisli
import Profunctor.Product
import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Muxable
import Profunctor.Lazy

import Monoidal.Applicative
import Monoidal.Alternative

import Optics

type Parser    f   = Joker (StateT String f)
type Printer   f   = Kleisli (WriterT String f)
type Biparser  f   = Product (Parser f) (Printer f)
type Biparser' f x = Biparser f x x

biparser :: StateT String f o -> (i -> WriterT String f o) -> Biparser f i o
biparser x y = Pair (Joker x) (Kleisli y)

biparse :: Biparser f i o -> String -> f (o, String)
biparse = runStateT . runJoker . pfst

biparse_ :: Functor f => Biparser f i o -> String -> f o
biparse_ bp = fmap fst . biparse bp

biprint :: Biparser f i o -> i -> f (o, String)
biprint = (runWriterT .) . runKleisli . psnd

biprint_ :: Functor f => Biparser f i o -> i -> f String
biprint_ bp = fmap snd . biprint bp

char :: Biparser' Maybe Char
char = biparser r w
  where
  r = get >>= \case { [] -> empty; (c : xs) -> c <$ put xs }
  w c = c <$ tell [c]

string :: String -> Biparser' Maybe String
string s = re (exactly s) $ bundle $ replicate (length s) char

token :: String -> x -> Biparser Maybe a x
token s x = dimap (const s) (const x) $ string s

token_ :: String -> Biparser Maybe a ()
token_ s = token s ()

trivial :: Biparser' Maybe ()
trivial = start

perhaps :: Biparser' Maybe x -> Biparser' Maybe (Maybe x)
perhaps p = m2e . symmE $ p \/ trivial

separated :: Biparser Maybe () x -> Biparser' Maybe y -> Biparser' Maybe (NonEmpty y)
separated s v = rec
  where
  rec = asNE $ ((v /- s) /\ (defer $ \_ -> rec)) \/ v
    where
    asNE :: Iso' (NonEmpty x) ((x, NonEmpty x) + x)
    asNE = iso fwd bwd
      where
      fwd (NE.uncons -> (x, xs)) = case xs of { Nothing -> Right x; Just xs' -> Left (x, xs') }
      bwd = either (uncurry NE.cons) pure

commaSeparated :: Biparser' Maybe x -> Biparser' Maybe (NonEmpty x)
commaSeparated = separated (token_ ",")

sign :: Biparser' Maybe Bool
sign = perhaps (re (exactly '-') char) & dimap sign2char char2sign
  where
  sign2char True = Nothing
  sign2char False = Just '-'

  char2sign (Just _) = False
  char2sign Nothing = True

digit :: Biparser' Maybe DecDigit
digit = re (convert charDecimal) $ char

nat :: Biparser' Maybe Natural
nat = re (asNonEmpty . digits2int) $ each digit

i2n :: Prism' Int Natural
i2n = prism naturalToInt (\i -> if i < 0 then Left i else Right (intToNatural i))

int :: Biparser' Maybe Int
int = do
  s <- sign & lmap (\i -> i >= 0)
  n <- nat & lmap (intToNatural . abs)
  let i = naturalToInt n
  pure $ if s
    then i
    else -i

hexDigit :: Biparser' Maybe HeXDigit
hexDigit = re (convert charHeXaDeCiMaL) char
