{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class RevAdd as where
	revAdd :: as -> as -> as

instance RevAdd [a] where
	revAdd xs ys = rxs ++ ys
		where
		rxs :: [a]
		rxs = reverse xs
