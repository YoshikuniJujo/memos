{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Eval e where
	foo :: e -> String

eval :: forall a b c . Eval (a -> (b, c)) => a -> b
eval = const undefined (undefined :: c)

instance Eval (Int -> (Int, Int)) where
	foo f = show $ f 0
