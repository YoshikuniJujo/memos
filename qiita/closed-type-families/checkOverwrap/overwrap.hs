{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Hige = Hige

class Foo f where
	foo :: f -> String

-- instance Foo f where
instance {-# OVERLAPPABLE #-} Foo f where
	foo _ = "foo"

instance Foo Hige where
-- instance {-# OVERLAPPING #-} Foo Hige where
	foo _ = "hige"

class Bar b where
	bar :: b -> String

instance Bar b where
-- instance {-# INCOHERENT #-} Bar b where
	bar _ = "bar"

-- instance Bar Hige where
instance {-# INCOHERENT #-} Bar Hige where
	bar _ = "hige"

class Baz b where
	baz :: b -> String

{-
instance Baz Hige where
	baz _ = "hige"

instance {-# INCOHERENT #-} Baz Hige where
	baz _ = "whip"
	-}

class Qux a b where
	qux :: a -> b -> String

instance Qux Char b where
	qux _ _ = "qux"

instance {-# INCOHERENT #-} Qux a Char where
	qux _ _ = "quxqux"
