{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Foo a where
	FooInt :: Int -> Foo Int
	FooBool :: Bool -> Foo Bool
--	deriving Show

fromFoo :: Foo a -> a
fromFoo (FooInt n) = n
fromFoo (FooBool b) = b

toFooGen :: a -> Foo a -> Foo a
toFooGen n (FooInt _) = FooInt n
toFooGen b (FooBool _) = FooBool b

fun :: a -> Foo a -> a
fun n1 (FooInt n2) = n1 + n2
fun b1 (FooBool b2) = b1 || b2

fun2 :: a -> Foo a -> Foo a
fun2 n1 (FooInt n2) = FooInt $ n1 + n2
fun2 b1 (FooBool b2) = FooBool $ b1 || b2
