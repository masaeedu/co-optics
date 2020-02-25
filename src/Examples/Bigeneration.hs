module Examples.Bigeneration where

import MyPrelude

import Test.QuickCheck hiding (arbitrary)
import qualified Test.QuickCheck as Q (arbitrary)

import Profunctor.Joker
import Profunctor.Kleisli
import Profunctor.Product
import Profunctor.Mux

import Monoidal.Applicative

import Optics

type Generator      = Joker Gen
type Shrink         = Kleisli Maybe
type Bigenerator    = Product Generator Shrink
type Bigenerator' x = Bigenerator x x

bigenerator :: Gen o -> (i -> Maybe o) -> Bigenerator i o
bigenerator x y = Pair (Joker x) (Kleisli y)

bigenerate :: Bigenerator i o -> Gen o
bigenerate = runJoker . pfst

bishrink :: Bigenerator i o -> i -> Maybe o
bishrink = runKleisli . psnd

arbitrary :: Arbitrary a => Bigenerator a a
arbitrary = bigenerator Q.arbitrary Just

testBigeneration :: IO ()
testBigeneration = do
  let zeroToFive = re (bounded 0 5) arbitrary
  sample $ bigenerate $ zeroToFive
  sample $ bigenerate $ zeroToFive /\ zeroToFive
  pure ()
