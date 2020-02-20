module Main where

import Data.Profunctor (Profunctor(..), Strong(..), Costrong(..), Choice(..), Cochoice(..))
import Control.Arrow (Kleisli(..))
import Control.Applicative (Alternative(..))
import Control.Arrow (arr)
import Data.Bifunctor (first, second)
import Data.Bifunctor.Joker (Joker(..))

class Functor f => Filterable f
  where
  partition :: f (Either a b) -> (f a, f b)

instance Filterable []
  where
  partition [] = ([], [])
  partition (Left x : xs) = first (x :) $ partition xs
  partition (Right x : xs) = second (x :) $ partition xs

instance (Filterable m) => Cochoice (Joker m)
  where
  unleft (Joker m) = Joker $ fst $ partition m

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

instance Strong p => Costrong (Re p s t)
  where
  unfirst (Re p) = Re $ p . first'

instance Costrong p => Strong (Re p s t)
  where
  first' (Re p) = Re $ p . unfirst

type Optic p  s t a b = p a b -> p s t

type Iso      s t a b = forall p. Profunctor p => Optic p s t a b
type Lens     s t a b = forall p. Strong     p => Optic p s t a b
type Prism    s t a b = forall p. Choice     p => Optic p s t a b
type Coprism  s t a b = forall p. Cochoice   p => Optic p s t a b

type Iso'     s a = Iso     s s a a
type Prism'   s a = Prism   s s a a
type Coprism' s a = Coprism s s a a

re :: Optic (Re p a b) s t a b -> Optic p b a t s
re p = runRe $ p $ Re id

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build match = dimap match (either id build) . right'

coprism :: (s -> a) -> (b -> Either a t) -> Coprism s t a b
coprism cobuild comatch = unright . dimap (either id cobuild) comatch

_int :: Prism' Double Int
_int = prism b m
  where
  b = fromIntegral
  m f =
    let r = round f
    in if fromIntegral r == f then Right r else Left f

main :: IO ()
main = do
  -- Since our arrow supports failure, we can work with integers as if they were doubles
  print $ runKleisli @Maybe (re _int $ arr (/ 2)) $ 2 -- Just 1
  print $ runKleisli @Maybe (re _int $ arr (/ 2)) $ 3 -- Nothing

  -- Another, slightly stupider notion of "arrow" is one with no input
  print $ runJoker $ re _int $ Joker $ [42, 4.2, 5, 1.5]  -- [42, 5]
