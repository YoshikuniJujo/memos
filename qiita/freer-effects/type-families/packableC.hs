{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

class Packable p where
--	type List p
	type family List p
	fromList :: [p] -> List p
	toList :: List p -> [p]

instance Packable () where
	type List () = Int
--	type instance List () = Int
	fromList = length
	toList = (`replicate` ())

instance Packable Bool where
	type List Bool = (Int, Integer)
	fromList = \case
		[] -> (0, 0)
		b : bs -> (+ 1) *** (bool 0 1 b .|.) . (`shiftL` 1)
			$ fromList bs
	toList (l, n) | l <= 0 || n < 0 = []
	toList (l, n) = n `testBit` 0 : toList (l - 1, n `shiftR` 1)

instance Packable Word8 where
	type List Word8 = BS.ByteString
	fromList = BS.pack
	toList = BS.unpack

instance Packable Double where
	type List Double = [Double]
	fromList = id
	toList = id

{-
type instance List Int = [Int]

instance Packable Int where
	fromList = id
	toList = id
	-}
