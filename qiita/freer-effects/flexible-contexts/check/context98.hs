{-# LANGUAGE Haskell98 #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
	foo :: a -> String

some :: Foo (m Integer) => m a -> String
-- some :: Foo (m a) => m a -> String
-- some :: Foo (m b) => m a -> String
some _ = "some"

instance Foo (IO a) where
	foo _ = "hello"

data Show (m (a, b)) => Bar m a b c = Bar (m (a, b)) c

instance Show (m (a, b)) => Show (Bar m a b c) where
	show (Bar x _) = "Bar " ++ show x ++ " _"

class Show (m (Integer, Double)) => Baz m where
