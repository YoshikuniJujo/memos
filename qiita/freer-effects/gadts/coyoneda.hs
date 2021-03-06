{-# LANGUAGE ExistentialQuantification, GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)

coyoneda :: t x -> Coyoneda t x
coyoneda tx = Coyoneda tx id

instance Functor (Coyoneda t) where
	fmap f (Coyoneda tx g) = Coyoneda tx $ f . g

data Reader r a where
	Reader :: Reader r r

{-
data family Reader r a where
	Reader r r = Reader
-}
