{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce

data UnionValue = forall x . UnionValue x

hetero :: [UnionValue]
hetero = [
	UnionValue (123 :: Integer), UnionValue True,
	UnionValue (), UnionValue 'c']

fromHetero :: [UnionValue] -> (Integer, Bool, (), Char)
fromHetero [UnionValue n, UnionValue b, UnionValue u, UnionValue c] = (
	unsafeCoerce n, unsafeCoerce b, unsafeCoerce u, unsafeCoerce c )
fromHetero _ = error "This function is only for `hetero'"

doubleValue :: UnionValue
doubleValue = UnionValue (123 :: Double)
