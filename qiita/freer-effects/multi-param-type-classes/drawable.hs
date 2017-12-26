{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.ByteString

class Drawable a b where
	draw1 :: b -> Maybe (a, b)

instance Drawable Char ByteString where
