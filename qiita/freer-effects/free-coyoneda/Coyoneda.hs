{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Coyoneda where

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)

coyoneda :: t a -> Coyoneda t a
coyoneda = (`Coyoneda` id)

instance Functor (Coyoneda t) where
	fmap f (Coyoneda tx g) = Coyoneda tx $ f . g

adenoyoc :: Functor f => Coyoneda f a -> f a
adenoyoc (Coyoneda fx f) = f <$> fx
