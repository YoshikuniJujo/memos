{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Foo a b where
	Foo t t = [t]
	Foo t s = ([t], [s])

class Bar b where
	bar :: b -> 

instance Bar (Foo t t) where
	bar 
