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

instance Mulable BS.ByteString Double where
	bs * x = BS.take l . BS.concat $ replicate (ceiling x) bs
		where l = round $ fromIntegral (BS.length bs) Prelude.* x

instance Mulable Int Int where
	n * m = n Prelude.* m

instance Mulable Int Integer where
	n * m = n Prelude.* fromInteger m

instance Mulable Int Double where
	n * x = round $ fromIntegral n Prelude.* x

threeHello :: BS.ByteString
threeHello = "Hello" * (3 :: Int)

sevenHello :: BS.ByteString
sevenHello = "Hello" * (7 :: Integer)

threePointFourHello :: BS.ByteString
threePointFourHello = "Hello" * (3.4 :: Double)

threeFive :: Int
threeFive = 5 * (3 :: Int)

sevenFive :: Int
sevenFive = 5 * (7 :: Integer)

threePointFourFive :: Int
threePointFourFive = 5 * (3.4 :: Double)
