-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-tabs -fwarn-unticked-promoted-constructors #-}

type family ConsList (a :: [*])

type instance ConsList '[] = ()
type instance ConsList (a ': as) = (a, ConsList as)

icdb :: ConsList [Integer, Char, Double, Bool]
icdb = (123, ('c', (3.5, (True, ()))))
	where

type family BoolArg (b :: Bool)
type instance BoolArg 'False = Integer
type instance BoolArg 'True = Double

data family BoolArgD (b :: Bool)
data instance BoolArgD 'False = BADI Integer
data instance BoolArgD 'True = BADD Double

{-
data family Not (b :: Bool) ::

class Flippable b where
	fl :: BoolArgD b -> BoolArgD (Not b)

instance Flippable 'False where
	fl (BADI x) = BADD $ fromInteger x
	-}
