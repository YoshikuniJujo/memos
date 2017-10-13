{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

revAdd0 :: [a] -> [a] -> [a]
revAdd0 xs ys = rxs ++ ys
	where
	rxs = reverse xs

revAdd0' :: (forall a . [a] -> [a] -> [a])
revAdd0' xs ys = rxs ++ ys
	where
	rxs = reverse xs

{-
revAdd1 :: [a] -> [a] -> [a]
revAdd1 xs ys = rxs ++ ys
	where
	rxs :: [a]
	rxs = reverse xs

revAdd2 :: (forall a . [a] -> [a] -> [a])
revAdd2 xs ys = rxs ++ ys
	where
	rxs :: (forall a . [a])
	rxs = reverse xs
	-}
