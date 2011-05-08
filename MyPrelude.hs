{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (error)
import GHC.Num ((+),(*),(-))

-- Basic
id :: a -> a
id a = a

-- Functor
class Functor f where
  fmap, (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap
  (<$) :: a -> f b -> f a
  (<$) a x = fmap (\_ -> a) x

-- Applicative
class Functor f => Applicative f where
  return, pure :: a -> f a
  return = pure
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (*>) x y = ((\_ -> id) <$> x) <*> y
  (<*) :: f a -> f b -> f a
  (<*) x y = ((\i -> (\_ -> i)) <$> x) <*> y

-- Monad
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  -- (>>) :: m a -> m b -> m b
  -- fail :: String -> m a

-- [] (Functor,Monad,Eq,Ord,Read,Show)
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

-- IO

-- Maybe

-- State


-- etc
head :: [a] -> a
head (x:xs) = x
