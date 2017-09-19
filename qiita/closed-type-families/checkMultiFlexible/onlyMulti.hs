{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a b where
	fun :: a -> b

instance Foo Int Char where
	fun = toEnum
