{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
	f :: a -> String


instance Foo [a] where
	f _ = "instance Foo [a] where"

instance Foo [Integer] where
	f _ = "instance Foo [Integer] where"

main = putStrLn $ f [3, 4, 5]
