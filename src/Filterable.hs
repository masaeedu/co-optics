module Filterable where

import Data.Bifunctor (first, second)
import Data.Map.Strict (Map, foldrWithKey, empty, insert)

class Functor f => Filterable f
  where
  partition :: f (Either a b) -> (f a, f b)

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

instance Ord k => Filterable (Map k)
  where
  partition = foldrWithKey (\k -> either (first . insert k) (second . insert k)) (empty, empty)
