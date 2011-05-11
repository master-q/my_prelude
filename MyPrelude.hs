{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (error)
import GHC.Num ((+),(*),(-))
import GHC.Show
-- import GHC.Types (Char) -- これを使ってしまうとIOが定義されちゃう。。。

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
  (>>) :: m a -> m b -> m b
  (>>) m n = m >>= (const n)
  (=<<) :: (a -> m b) -> m a -> m b
  (=<<) f m = m >>= f
  -- fail :: String -> m a -- <= いつ使うの？

-- [] --------------------------------------------------
-- data [] a = [] | a : [a] -- <= []自体の定義は構文糖でいいのかな？

(++) :: [a] -> [a] -> [a]
(++) [] y = y
(++) (x:xs) y = x : (xs ++ y)

instance Functor [] where
  fmap f [] = []
  fmap f (x:xs) = (f x) : (fmap f xs)

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:xs) = (f x) ++ (concatMap f xs)

instance Applicative [] where
  pure a = [a]
  (<*>) [] _ = []
  (<*>) (x:xs) y = (x <$> y) ++ (xs <*> y)

instance Monad [] where
  (>>=) m f = concatMap f m -- なんかdo構文にするとココを使わないみたい。。。

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

instance Monad Maybe where
  (>>=) Nothing _ = Nothing
  (>>=) (Just x) f = f x

-- State -----------------------------------------------
newtype State s a = State { runState :: (s -> (a,s)) }

-- instance Functor (State s) where

-- IO --------------------------------------------------
data World = World
type IO a = World -> (a, World) -- <= newtypeとtypeの違いは？
-- xxx TODO: instance Functor IO where
-- xxx TODO: instance Applicative IO where
-- xxx TODO: instance Monad IO where

{--
type String = [Char]

putStrLn :: String -> IO ()
putStrLn s = error s -- <= これ以上どうしろと。。。

getLine_fake :: String -> IO String
getLine_fake s = IO s
--}
