module Monoidal.Filterable where

import MyPrelude

import Data.Bifunctor (first, second, bimap)
import Data.Tuple (swap)

import Control.Monad.State.Lazy (StateT(..))
import Control.Monad.Writer.Lazy (WriterT(..))

import qualified Data.Map.Strict as M (Map, foldrWithKey, empty, insert)

import Hedgehog

import Monoidal.Applicative
import Monoidal.Alternative
import Monoidal.Decisive

class Functor f => Filterable f
  where
  partition :: f (a + b) -> f a × f b

newtype Dynamic f a = Dynamic { runDynamic :: f a }
  deriving (Functor, Apply, Applicative, Alt, Alternative, Monad)

instance (Monad m, Applicative m, Alternative m) => Filterable (Dynamic m)
  where
  partition m = (m >>= either pure (const empty), m >>= either (const empty) pure)

instance Filterable []
  where
  partition [] = ([], [])
  partition (Left x : xs) = first (x :) $ partition xs
  partition (Right x : xs) = second (x :) $ partition xs

instance Filterable Maybe
  where
  partition Nothing = (Nothing, Nothing)
  partition (Just (Left a)) = (Just a, Nothing)
  partition (Just (Right b)) = (Nothing, Just b)

instance Ord k => Filterable (M.Map k)
  where
  partition = M.foldrWithKey (\k -> either (first . M.insert k) (second . M.insert k)) (M.empty, M.empty)

trivial :: Functor f => f (a, b) -> (f a, f b)
trivial fab = (fmap fst fab, fmap snd fab)

partitionInner :: (Functor f, Filterable g) => f (g (a + b)) -> f (g a) × f (g b)
partitionInner = trivial . fmap partition

partitionOuter :: (Filterable f, Decide g) => f (g (a + b)) -> f (g a) × f (g b)
partitionOuter = partition . fmap decide

instance Filterable m => Filterable (StateT s m)
  where
  partition = bimap bwd bwd . trivial . fmap partitionOuter . fwd
    where
    fwd :: Functor m => StateT s m a -> s -> m (s, a)
    fwd = (fmap swap .) . runStateT

    bwd :: Functor m => (s -> m (s, a)) -> StateT s m a
    bwd = StateT . (fmap swap .)

instance Filterable m => Filterable (WriterT w m)
  where
  partition = bimap bwd bwd . partitionOuter . fwd
    where
    fwd :: Functor m => WriterT w m a -> m (w, a)
    fwd= fmap swap . runWriterT

    bwd :: Functor m => m (w, a) -> WriterT w m a
    bwd = WriterT . fmap swap

deriving via (Dynamic Gen) instance Filterable Gen
