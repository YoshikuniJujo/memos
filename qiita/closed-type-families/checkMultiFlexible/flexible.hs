{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Foo = Foo

-- instance {-# OVERLAPPABLE #-} Show s where
instance Show s where
	show _ = "hoge"

-- instance {-# OVERLAPPABLE #-} Show Foo where
instance Show Foo where
	show _ = "Foo"
