{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

prefix :: forall a . a -> [[a]] -> [[a]]
prefix x yss = map xcons yss
	where
	xcons :: [a] -> [a]
	xcons ys = x : ys

prefix2 :: a -> [[a]] -> [[a]]
prefix2 (x :: b) yss = map xcons yss
	where
	xcons :: [b] -> [b]
	xcons ys = x : ys

some :: a -> a
some = id

some2 :: (forall a . a -> a)
some2 = id

some3 :: forall a . a -> a
some3 = id

add :: forall a . [a] -> [a] -> [a]
add xs ys = rxs ++ ys
	where
	rxs :: [a]
	rxs = reverse xs

add2 :: [a] -> [a] -> [a]
add2 (xs :: [b]) ys = rxs ++ ys
	where
	rxs :: [b]
	rxs = reverse xs

add3 :: [a] -> [a] -> [a]
add3 xs ys :: [b] = rxs ++ ys
	where
	rxs :: [b]
	rxs = reverse xs
