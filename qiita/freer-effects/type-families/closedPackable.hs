{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

type family List x where
	List () = Int
--	List Bool = (Int, Integer)
--	List Word8 = BS.ByteString
	List a = [a]

class Packable p where
	fromList :: [p] -> List p
	toList :: List p -> [p]

instance Packable () where
	fromList = length
	toList = (`replicate` ())

{-
instance {-# OVERLAPPABLE #-} Packable a where
	fromList = id
	toList = id
	-}
