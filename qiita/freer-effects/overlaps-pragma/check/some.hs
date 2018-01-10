{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a b where
	f :: a -> b -> String

newtype Boo a = Boo a

instance {-# OVERLAPPABLE #-} Foo (Boo a) b where
	f _ _ = "Foo (Boo a) b"

instance {-# OVERLAPPABLE #-} Foo (Boo Bool) b where
	f _ _ = "Foo (Boo Bool) b"

instance {-# INCOHERENT #-} Foo (Boo Bool) Integer where
	f _ _ = "Foo (Boo Bool) Integer"

some :: Boo Bool -> b -> String
some x y = f x y

main :: IO ()
main = do
	putStrLn $ f (Boo True) (123 :: Integer)
	putStrLn $ some (Boo True) (123 :: Integer)
