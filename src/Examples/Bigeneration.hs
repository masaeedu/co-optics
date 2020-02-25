module Examples.Bigeneration where

import MyPrelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad

import Profunctor.Joker
import Profunctor.Kleisli
import Profunctor.Product
import Profunctor.Mux
import Profunctor.Demux
import Profunctor.Muxable

import Monoidal.Applicative

import Optics
type Generator      = Joker Gen
type Check          = Kleisli Maybe
type Bigenerator    = Product Generator Check
type Bigenerator' x = Bigenerator x x

bigenerator :: Gen o -> (i -> Maybe o) -> Bigenerator i o
bigenerator x y = Pair (Joker x) (Kleisli y)

bigenerate :: Bigenerator i o -> Gen o
bigenerate = runJoker . pfst

bicheck :: Bigenerator i o -> i -> Maybe o
bicheck = runKleisli . psnd

sample :: Show x => Gen x -> IO ()
sample = print <=< Gen.sample

int :: Bigenerator Int Int
int = bigenerator (Gen.int (Range.constant 0 10)) Just

testBigeneration :: IO ()
testBigeneration = do
  let twoToFive = re (bounded 2 5) $ int

  sample $ bigenerate $ twoToFive -- [4, 3, 5]
  print $ bicheck twoToFive $ 8 -- Nothing
  print $ bicheck twoToFive $ 4 -- Just 4

  let twoToFives = twoToFive /\ twoToFive

  sample $ bigenerate $ twoToFives -- [(2,3),(5,2),(4,4)]
  print $ bicheck twoToFives $ (8, 1) -- Nothing
  print $ bicheck twoToFives $ (2, 4) -- Just (2, 4)

  let sevenToTen = re (bounded 7 10) int
  let ranges = twoToFive \/ sevenToTen

  sample $ bigenerate $ ranges
  sample $ bigenerate $ bundle $ replicate 2 ranges

  pure ()
