{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (error)
import GHC.Num ((+),(*),(-))
import GHC.Show

-- Basic
id :: a -> a
id a = a

const :: a -> b -> a
const x _ = x

-- Functor ---------------------------------------------
class Functor f where
  fmap, (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap
  (<$) :: a -> f b -> f a
  (<$) a x = fmap (\_ -> a) x

-- Applicative -----------------------------------------
class Functor f => Applicative f where
  return, pure :: a -> f a
  return = pure
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (*>) x y = ((\_ -> id) <$> x) <*> y
  (<*) :: f a -> f b -> f a
  (<*) x y = (const <$> x) <*> y

-- Monad -----------------------------------------------
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  -- (>>) :: m a -> m b -> m b
  -- fail :: String -> m a

-- [] --------------------------------------------------
-- data [] a = [] | a : [a] -- <= []自体の定義は構文糖でいいのかな？
instance Functor [] where
  fmap f [] = []
  fmap f (x:xs) = (f x) : (fmap f xs)
instance Applicative [] where
  pure a = [a] -- これでいいのか？
  (<*>) [] _ = []
  (<*>) (x:xs) y = (x <$> y) `cat` (xs <*> y)
    where
      cat :: [a] -> [a] -> [a]
      cat [] y = y
      cat (x:xs) y = x : (xs `cat` y)
-- xxx TODO: instance Monad [] where

-- Maybe -----------------------------------------------
data Maybe a = Nothing | Just a
             deriving (Show)
instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
instance Applicative Maybe where
  pure a = Just a
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just x) y = x <$> y

-- IO --------------------------------------------------


-- State -----------------------------------------------


-- etc
head :: [a] -> a
head (x:xs) = x
