{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Example a where
	Some :: Int -> Example Int
	Other :: Bool -> Example Bool

fun :: Example a -> a
fun (Some n) = n
fun (Other b) = b

class SomeClass a where
	fun2 :: Example a -> a

instance SomeClass Int where
	fun2 (Some n) = n

instance SomeClass Bool where
	fun2 (Other b) = b
