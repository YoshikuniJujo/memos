{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Nil
data a :^ b
infixr 5 :^

type family TupleList a where
	TupleList Nil = ()
	TupleList (a :^ b) = (a, TupleList b)

ex1 :: TupleList (Integer :^ Double :^ Bool :^ Nil)
ex1 = (123, (3.14, (True, ())))
