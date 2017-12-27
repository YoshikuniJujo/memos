{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Char

class Totalable a where
	total :: a -> Integer

instance Totalable [Integer] where
	total = sum

instance Totalable [String] where
	total [] = 0
	total (s : ss)
		| all isDigit s = read s + total ss
		| otherwise = total ss
