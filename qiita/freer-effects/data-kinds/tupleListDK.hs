{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data List a = Nil | a :^ (List a)
infixr 5 :^

type family TList a where
	TList 'Nil = ()
	TList (a ':^ b) = (a, TList b)

ex1 :: TList (Integer ':^ Double ':^ Bool ':^ 'Nil)
ex1 = (123, (3.14, (True, ())))
