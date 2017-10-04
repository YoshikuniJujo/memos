{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Free t a
	= Pure a
	| Join (t (Free t a))

type Coyoneda t a = forall x . (t x, x -> a)
