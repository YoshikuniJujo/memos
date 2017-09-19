{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-
type family Foo x where
	Foo () = Int
	Foo a = [a]

class Bar b where
	bar :: b -> Foo b

instance Bar () where
	bar _ = 123

instance {-# OVERRAPPABLE #-} Bar a where
	bar = (: [])
	-}

{-
class Bar b where
	type Foo b
	bar :: b -> Foo b

instance Bar () where
	type Foo () = Int
	bar _ = 123

instance {-# OVERLAPPABLE #-} Bar a where
	type Foo a = [a]
	bar = (: [])
	-}
