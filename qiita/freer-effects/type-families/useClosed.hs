{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Empty

type family Foo x where
	Foo Int = Bool
	Foo a = String

class UseFoo u where
	useFoo :: u -> Foo u

instance UseFoo Int where
	useFoo = (== 0)

instance UseFoo Double where
	useFoo x = show x

type family Bar x where
	Bar Int = Bool
	Bar Double = Integer
	Bar Char = Double
	Bar a = Empty

{-
type family Baz x y where
	Baz Int Double = Bool
	Baz 
	-}
