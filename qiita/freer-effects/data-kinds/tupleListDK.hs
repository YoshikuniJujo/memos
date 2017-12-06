{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data List a = Nil | a :^ (List a)
infixr 5 :^

type family TupleList a where
	TupleList 'Nil = ()
	TupleList (a ':^ b) = (a, TupleList b)

ex1 :: TupleList (Integer ':^ Double ':^ Bool ':^ 'Nil)
ex1 = (123, (3.14, (True, ())))
