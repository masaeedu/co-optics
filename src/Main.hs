module Main where

import Data.Profunctor (Profunctor(..), Choice(..), Cochoice(..))
import Control.Arrow (Kleisli(..))
import Control.Applicative (Alternative(..))
import Control.Arrow (arr)

instance (Monad m, Alternative m) => Cochoice (Kleisli m)
  where
  unleft (Kleisli amb) = Kleisli $ \a -> amb (Left a) >>= either pure (const empty)

newtype Re p s t a b = Re { runRe :: p b a -> p t s }

instance Profunctor p => Profunctor (Re p s t)
  where
  dimap f g (Re p) = Re $ p . dimap g f

instance Choice p => Cochoice (Re p s t)
  where
  unleft (Re p) = Re $ p . left'

instance Cochoice p => Choice (Re p s t)
  where
  left' (Re p) = Re $ p . unleft

type Optic p  s t a b = p a b -> p s t
type Prism    s t a b = forall p. Choice p   => Optic p s t a b
type Prism'   s   a   = Prism s s a a
type Coprism  s t a b = forall p. Cochoice p => Optic p s t a b
type Coprism' s   a   = Coprism s s a a

re :: Optic (Re p a b) s t a b -> Optic p b a t s
re p = runRe $ p $ Re id

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build match = dimap match (either id build) . right'

coprism :: (s -> a) -> (b -> Either a t) -> Coprism s t a b
coprism cobuild comatch = unright . dimap (either id cobuild) comatch

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = dimap (maybe (Right ()) Left) (either Just (const Nothing)) . left'

_whole :: Prism' Double Int
_whole = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

-- We want to work dangerously in the domain of real numbers
divby2 :: Kleisli Maybe Double Double
divby2 = arr (/ 2)

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with input that lives in the domain of integers
  print $ runKleisli (re _whole divby2) $ 2
  print $ runKleisli (re _whole divby2) $ 3
