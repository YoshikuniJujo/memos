{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Revable a where
	rev :: a -> a

instance Revable (a, a) where
	rev (x, y) = (y, x)

instance Revable (Either a a) where
	rev (Left x) = Right x
	rev (Right y) = Left y

tpl :: (Char, Char)
tpl = ('j', 'z')

ethr :: Either Double Double
ethr = Left 1.23
