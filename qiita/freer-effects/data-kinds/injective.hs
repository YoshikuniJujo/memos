{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Bad b where
	type Ba b = r | r -> b
	bad :: Ba b -> Ba b

instance Bad Integer where
	type Ba Integer = Double
	bad = id

{-
instance Bad Bool where
	type Ba Bool = Double
	bad = id
	-}
