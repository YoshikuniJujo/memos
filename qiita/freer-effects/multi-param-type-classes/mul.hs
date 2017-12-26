{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding ((*))
import qualified Prelude
import qualified Data.ByteString as BS

class Mulable a b where
	(*) :: a -> b -> a

instance Mulable BS.ByteString Int where
	bs * n = BS.concat $ replicate n bs

instance Mulable BS.ByteString Integer where
	bs * n = BS.concat $ replicate (fromInteger n) bs

instance Mulable Int Int where
	n * m = n Prelude.* m

instance Mulable Int Integer where
	n * m = n Prelude.* fromInteger m

instance Mulable Double Int where
	x * n = x Prelude.* fromIntegral n

threeHello :: BS.ByteString
threeHello = "hello" * (3 :: Int)

sevenHello :: BS.ByteString
sevenHello = "hello" * (7 :: Integer)

threeFive :: Int
threeFive = 5 * (3 :: Int)

sevenFive :: Int
sevenFive = 5 * (7 :: Integer)
