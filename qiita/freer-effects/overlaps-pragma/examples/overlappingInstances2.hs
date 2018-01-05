{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a b where
	f :: a -> b -> String

instance Foo Integer b where
	f _ _ = "instance Foo Integer b where"

instance Foo a Char where
	f _ _ = "instance Foo a Char where"
