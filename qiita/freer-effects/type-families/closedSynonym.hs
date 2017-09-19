{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Foo x where
	Foo Int = Char
	Foo Double = Bool

-- type instance Foo () = String
