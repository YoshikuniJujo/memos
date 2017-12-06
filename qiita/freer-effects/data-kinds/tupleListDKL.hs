{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family TupleList a where
	TupleList '[] = ()
	TupleList (a ': b) = (a, TList b)

ex1 :: TupleList '[Integer, Double, Bool]
ex1 = (123, (3.14, (True, ())))
