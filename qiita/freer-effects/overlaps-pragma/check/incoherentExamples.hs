{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class C a b where
	f :: a -> b -> String

instance {-# INCOHERENT #-} C Char b where
	f _ _ = "instance C Char b"

-- instance {-# INCOHERENT #-} C a Bool where
instance C a Bool where
	f _ _ = "instance C a Bool"
