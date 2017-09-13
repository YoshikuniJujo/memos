-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infixr 9 :.:
-- data FunList a b = Id | forall x . (x -> b) :.: FunList a x
data FunList a b where
	Id :: FunList a a
	(:.:) :: (x -> b) -> FunList a x -> FunList a b

sample :: FunList Integer String
sample = show :.: (* 3) :.: (+ 5) :.: Id

apply :: FunList a b -> a -> b
apply Id = id
apply (f :.: fs) = f . apply fs
