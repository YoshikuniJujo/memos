{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class IsConvertableInto a b where
	convert :: a -> b

instance IsConvertableInto Integer Bool where
	convert 0 = False
	convert _ = True

instance IsConvertableInto Double Integer where
	convert = round
