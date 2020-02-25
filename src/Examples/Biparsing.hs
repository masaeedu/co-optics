{-# LANGUAGE LambdaCase #-}
module Examples.Biparsing where

import MyPrelude

import GHC.Natural (Natural, intToNatural, naturalToInt)

import Data.Profunctor (Profunctor(..))
import Data.Bifunctor.Product (Product(..))

import Control.Monad.State.Lazy (StateT(..), get, put)
import Control.Monad.Writer.Lazy (WriterT(..), tell)

import Data.Digit (DecDigit(..))

import Profunctor.Joker
import Profunctor.Kleisli
import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Product
import Profunctor.Muxable

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

biprint :: Biparser f i o -> i -> f (o, String)
biprint = (runWriterT .) . runKleisli . psnd

testBiparsing :: IO ()
testBiparsing = do
  let
    -- A biparser that parses one character
    char :: Biparser' Maybe Char
    char = biparser r w
      where
      r = get >>= \case { [] -> empty; (c : xs) -> c <$ put xs }
      w c = c <$ tell [c]

  -- It turns out we can get pretty far with this one humble biparser
  -- and a truckload of profunctor optics

  -- Firstly, given an arbitrary prism, we can run it backwards on
  -- a biparser to focus it onto the successful case of the prism

  -- For example, there is a prism from arbitrary characters to digits
  -- @@@
  -- c2d :: Prism' Char DecDigit
  -- @@@

  -- We can run this backwards on our @char@ biparser to get:
  let
    digit :: Biparser' Maybe DecDigit
    digit = re c2d $ char

  -- This can be used to parse a digit out of a string
  print $ biparse digit $ ""   -- > Nothing
  print $ biparse digit $ "c5" -- > Nothing
  print $ biparse digit $ "5c" -- > Just (DecDigit5, "c")

  -- Or to print a digit to a string
  print $ biprint digit $ DecDigit2 -- > Just (DecDigit2, "2")

  -- Ok, so that's well and good, but parsing single characters is no fun. We
  -- can use traversals to parse repeated instances of a pattern

  -- For example, we can use the @each@ traversal to build a biparser for a
  -- list of digits
  let
    digits :: Biparser' Maybe [DecDigit]
    digits = each digit

  -- Try it out forwards...
  print $ biparse digits $ "12..."                -- > Just ([DecDigit1, DecDigit2], "...")
  -- ...and backwards
  print $ biprint digits $ [DecDigit4, DecDigit2] -- > Just ([DecDigit4, DecDigit2], "42")

  -- All strings don't consist solely of digits though, so it's useful to be able
  -- to try out a number of different parsers at a given position, accepting the
  -- result of the first one that succeeds

  -- We can do this by taking the disjunction of a pair of biparsers. [1]

  -- E.g. we might want to first try parsing a digit, then fall back to parsing an
  -- arbitrary character
  let
    digitOrChar :: Biparser' Maybe (Either DecDigit Char)
    digitOrChar = digit \/ char

  -- Try it out on some inputs
  print $ biparse digitOrChar $ ""   -- > Nothing
  print $ biparse digitOrChar $ "5"  -- > Just (Left DecDigit5, "")
  print $ biparse digitOrChar $ "c2" -- > Just (Right 'c', "2")

  -- How about a mixed list of digits and chars?
  print $ biparse (each digitOrChar) $ "a1b" -- > Just ([Right 'a',Left DecDigit1,Right 'b'],"")

  -- For the sake of example, let's run our @digits@ biparser through more
  -- coprisms until it turns into a biparser of natural numbers
  let
    nat :: Biparser' Maybe Natural
    nat = re (asNonEmpty . digits2int) $ digits

  -- Let's see if it works
  print $ biparse nat $ "123131234" -- > Just (123131234,"")
  print $ biparse nat $ "123a123"   -- > Just (123, "a123")

  -- We can mix and match
  let intOrChar = nat \/ char
  print $ biparse intOrChar $ "123a123"        -- > Just (Left 123,"a123")
  print $ biparse (each intOrChar) $ "123a123" -- > Just ([Left 123,Right 'a',Left 123],"")

  -- Things are getting complicated, so as a sanity check, let's try running the last example backwards
  -- and see if it produces the original string
  print $ biprint (each intOrChar) $ [Left 123, Right 'a', Left 123] -- > Just ([Left 123,Right 'a',Left 123],"123a123")

  -- Ok great, so we know how to alternate between biparsers.

  -- Sometimes however, we want to take results from multiple distinct biparsers and
  -- stitch them into a composite data structure.

  -- To do this, we can take the conjunction of two biparsers [1]

  -- Here's an example where we use this technique to model a list
  let
    delimitedInts :: Biparser' Maybe [(Natural, Char)]
    delimitedInts = each $ nat /\ char

  print $ biparse delimitedInts $ "12,13,14."                  -- > Just ([(12,','),(13,','),(14,'.')],"")
  print $ biprint delimitedInts $ [(12,','),(13,','),(14,'.')] -- > Just ([(12,','),(13,','),(14,'.')],"12,13,14.")

  -- etc. etc. TODO: add more examples

  -- Another thing we might want to do is discard irrelevant structure. Let's see how we can use one-sided inverses
  -- for this purpose.

  -- Suppose we want to parse a string that has some leading space and then a character
  let
    char2space :: Prism' Char Char
    char2space = prism id (\case { ' ' -> Right ' '; c -> Left c })

    space :: Biparser' Maybe Char
    space = re char2space char

    spacesThenChar :: Biparser' Maybe (String, Char)
    spacesThenChar = each space /\ char

  -- It works like this
  print $ biparse spacesThenChar $ "  f" -- > Just (("  ",'f'),"")

  -- We can see that our result consists of a whitespace string and the character we parsed out.

  -- Let's say we think the spaces are useless, and we want to throw them away. A conservative change
  -- we can make is adjust the output end of the biparser, leaving the input end untouched
  let
    -- Note how the resulting biparser has a asymmetric type
    spacesThenChar' :: Biparser Maybe (String, Char) Char
    spacesThenChar' = rmap snd spacesThenChar

  -- Now our parsing output only contains the stuff we care about for further manipulation
  print $ biparse spacesThenChar' $ "  f" -- > Just ('f',"")

  -- However, when printing, we still need to specify the full structure, including the requisite
  -- number of spaces. These will show up in the printed output, although the structured output of
  -- the printer will forget about the spaces and only keep the character
  print $ biprint spacesThenChar' $ ("    ", 'f') -- > Just ('f',"    f")

  -- Suppose we REALLY don't care about these spaces, even for printing purposes. We just
  -- want to break the isomorphism and pretend even the printer doesn't even expect spaces

  -- We can use a split monomorphism (aka a one sided inverse) to discard this structure entirely
  let
    spacesThenChar'' :: Biparser' Maybe Char
    spacesThenChar'' = discard spacesThenChar
      where
      discard :: Iso' Char (String, Char)
      discard = iso ("", ) snd -- @snd . ("", ) = id@, but @("", ) . snd != id@

  -- Test it out
  print $ biparse spacesThenChar'' $ " f"   -- > Just ('f',"")
  print $ biparse spacesThenChar'' $ "   f" -- > Just ('f',"")
  print $ biprint spacesThenChar'' $ 'f'    -- > Just ('f',"f")

  -- Ok, so far we've been dealing with totally static parsing. Now let's look at dynamic (monadic)
  -- parsing

  -- Let's say we want to parse a number, followed by a space, followed by precisely that many characters
  -- Here's how we can do it
  let
    string :: Biparser' Maybe String
    string = do
      n  <- nat   & lmap (intToNatural . length)
      _  <- space & lmap (const ' ')
      cs <- char  & replicate (naturalToInt n) & bundle
      pure cs

  print $ biparse string $ "10 whoops"         -- > Nothing
  print $ biparse string $ "6 lambda calculus" -- > Just ("lambda"," calculus")
  print $ biprint string $ "SKI"               -- > Just ("SKI","3 SKI")

  -- [1] - These combinators actually don't have anything to do with biparsers specifically, biparsers are just one
  --       type of profunctor they can be instantiated at. See @Mux@ and @Demux@ in the repo for the general classes
