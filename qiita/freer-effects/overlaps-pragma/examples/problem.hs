{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
	f :: a -> String

instance Foo [a] where
	f _ = "It's list"

instance Foo String where
	f _ = "It's string"

class Bar a where
	g :: a -> String

instance Bar [a] where
	g _ = "It's list"

instance Bar String where
	g _ = "It's string"
