{-# LANGUAGE RebindableSyntax #-}
module Main where

import MyPrelude

import GHC.Natural

import Data.Profunctor (Profunctor(..))
import Data.Map.Strict (Map, fromList)
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Data.Digit (DecDigit, charDecimal)

import Optics

import Profunctor.Re ()
import Profunctor.Joker ()
import Profunctor.Mux
import Profunctor.Demux

import Monoidal.Filterable ()
import Monoidal.Applicative
import Monoidal.Alternative

import Parser

r2i :: Prism' Double Int
r2i = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

i2r :: Coprism' Int Double
i2r = re r2i

double :: Fractional n => Iso' n n
double = dimap (/ 2) (* 2)

halve :: Fractional n => Iso' n n
halve = re double

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with integers as if they were doubles
  print $ speculate i2r (Just . (/ 2)) $ 2 -- Just 1
  print $ speculate i2r (Just . (/ 2)) $ 3 -- Nothing

  -- Nonsense example where we find the factors of a number by just performing floating
  -- point division with prime factors
  let
    quotients :: Double -> Map Int Double
    quotients x = fromList [(1, x / 1), (2, x / 2), (3, x / 3), (5, x / 5)]

  -- Because a @Map@ is filterable, we can focus onto the integer results, discarding
  -- anything that didn't divide cleanly
  print $ speculate i2r quotients $ 15 -- fromList [(1, 15), (3, 5), (5, 3)]

  -- Another, slightly stupider notion of "arrow" is one with no input
  -- This basically corresponds to filtering a container
  let input = [42, 4.2, 5, 1.5]
  print $ i2r `whittle` input -- [42, 5]

  -- Coprisms interact appropriately with other optics
  print $ (i2r . halve) `whittle` input -- [21]

  let input' = ("foo",) <$> input
  print $ re (liftPrism r2i) `whittle` input' -- [("foo", 42), ("foo", 5)]

  -- There's some weird filterables out there
  -- E.g. parsers are filterable
  print $ flip (runStateT @String @Maybe) "0.5"
        $ re r2i `whittle`
          do
            s <- get
            pure $ read s -- Nothing

  let
    -- A biparser that parses one character
    -- It turns out this is actually all we need
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

    -- Same biparser run through a backwards prism @Prism' Char DecDigit@
    digit :: Biparser Maybe DecDigit DecDigit
    digit = re c2d $ char

    -- Run it through a traversal to get many digits
    digits :: Biparser Maybe [DecDigit] [DecDigit]
    digits = each digit

    -- Run it through more backwards prisms to get a natural number
    int :: Biparser Maybe Natural Natural
    int = re (asNonEmpty . digits2int) $ digits

  -- You can combine parsers* a couple of ways. First, you can take their disjunction.

  -- E.g. we might want to first try parsing a digit, then fall back to parsing a char
  let digitOrChar = digit \/ char

  print $ biparse digitOrChar $ ""   -- Nothing
  print $ biparse digitOrChar $ "5"  -- Just (Left DecDigit5, "")
  print $ biparse digitOrChar $ "c2" -- Just (Right 'c', "")

  -- Obviously we can keep making things more complicated
  -- How about a mixed list of digits and chars?
  print $ biparse (each digitOrChar) $ "a1b2c3d4" -- Just ([Right 'a',Left DecDigit1,Right 'b',Left DecDigit2,Right 'c',Left DecDigit3,Right 'd',Left DecDigit4],"")

  -- Let's try out that @int@ biparser we made
  print $ biparse int $ "123131234" -- Just (123131234,"")
  print $ biparse int $ "123a123"   -- Just (123, "a123")

  -- Let's mix integers and arbitrary characters
  let intOrChar = int \/ char
  print $ biparse intOrChar $ "123a123"        -- Just (Left 123,"a123")
  print $ biparse (each intOrChar) $ "123a123" -- Just ([Left 123,Right 'a',Left 123],"")

  -- Oh, btw, these are BIparsers. So they print too. Let's try running the last example backwards to see if we get the original string back
  print $ biprint (each intOrChar) $ [Left 123, Right 'a', Left 123] -- Just ([Left 123,Right 'a',Left 123],"123a123")

  -- Ok so much for disjunction. One other neat thing we can do is conjoin parsers
  -- so the stuff they're parsing out gets filled into some product type
  print $ biparse (each $ int /\ char) $ "12,13,14."                  -- Just ([(12,','),(13,','),(14,'.')],"")
  print $ biprint (each $ int /\ char) $ [(12,','),(13,','),(14,'.')] -- Just ([(12,','),(13,','),(14,'.')],"12,13,14.")

  -- * This is actually just a new pair of general purpose abstractions for profunctors, biparsers are just one instance
  -- See @Mux@ and @Demux@ in the repo
