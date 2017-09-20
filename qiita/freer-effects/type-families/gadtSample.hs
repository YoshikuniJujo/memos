{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Foo x where
	FooInt :: Bool -> Char -> Foo Int
	FooChar::  Double -> Integer -> Foo Char

class UseFoo a where
	useFoo :: a -> Foo a -> String

instance UseFoo Int where
	useFoo x (FooInt y z) = show x ++ " " ++ show y ++ " " ++ show z

instance UseFoo Char where
	useFoo x (FooChar y z) = show x ++ " " ++ show y ++ " " ++ show z

useF :: a -> Foo a -> String
useF x (FooInt y z) = show x ++ " " ++ show y ++ " " ++ show z
useF x (FooChar y z) = show x ++ " " ++ show y ++ " " ++ show z
