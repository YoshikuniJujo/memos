{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Join t a
	= Tip
	| Join (t (Join t a))

class Pure f where
	pr :: a -> f a

instance Pure [] where
	pr = (: [])
