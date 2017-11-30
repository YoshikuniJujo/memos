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
type instance BoolArg 'True = Integer
