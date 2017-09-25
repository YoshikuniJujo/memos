{-# LANGUAGE GADTSyntax #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Foo1 a b
	= Bar1 a
	| Baz1 b
	| Qux1 Int

data Foo2 a b where
	Bar2 :: a -> Foo2 a b
	Baz2 :: b -> Foo2 a b
	Qux2 :: Int -> Foo2 a b
