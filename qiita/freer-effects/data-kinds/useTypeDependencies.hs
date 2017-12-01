{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data F
data T

class Bar b where
	type Baz b = r | r -> b
	type Qux b
	bar :: Baz b -> Qux b

instance Bar F where
	type Baz F = Integer
	type Qux F = Double
	bar = fromInteger

instance Bar T where
	type Baz T = Double
	type Qux T = Integer
	bar = round
