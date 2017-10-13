{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

foo1 :: a -> b -> (a, b)
foo1 x y = (idA x, idB y)
	where
	idA :: a -> a
	idA = id
	idB :: a -> a
	idB = id

{-
foo2 :: forall a . a -> b -> (a, b)
foo2 x y = (idA x, idB y)
	where
	idA :: a -> a
	idA = id
	idB :: a -> a
	idB = id
	-}

foo3 :: (forall a . a -> b -> (a, b))
foo3 x y = (idA x, idB y)
	where
	idA :: a -> a
	idA = id
	idB :: a -> a
	idB = id

bar1 :: forall a b . a -> b -> (a, b)
bar1 x y = (idA x, idB y)
	where
	idA :: a -> a
	idA = id
	idB :: b -> b
	idB = id
