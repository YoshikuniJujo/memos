{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family TList a where
	TList '[] = ()
	TList (a ': b) = (a, TList b)

ex1 :: TList '[Integer, Double, Bool]
ex1 = (123, (3.14, (True, ())))
