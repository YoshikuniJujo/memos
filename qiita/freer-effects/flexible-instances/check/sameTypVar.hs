{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
	foo :: a -> String

instance Show a => Foo (Either a a) where
	foo (Left x) = "Left " ++ show x
	foo (Right y) = "Right " ++ show y
