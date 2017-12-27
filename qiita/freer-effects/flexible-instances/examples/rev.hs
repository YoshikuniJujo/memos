{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Revable a where
	rev :: a -> a

instance Revable [a] where
	rev = reverse

instance Revable (a, a) where
	rev (x, y) = (y, x)

instance Revable (Either a a) where
	rev (Left x) = Right x
	rev (Right y) = Left y

instance Revable (Maybe [a]) where
	rev Nothing = Just []
	rev (Just []) = Nothing
	rev mxs = mxs

lst :: [Integer]
lst = [4, 3, 1, 5, 2]

tpl :: (Char, Char)
tpl = ('j', 'z')

ethr :: Either Double Double
ethr = Left 1.23

mlst1, mlst2, mlst3 :: Maybe String
mlst1 = Nothing
mlst2 = Just ""
mlst3 = Just "hello"
