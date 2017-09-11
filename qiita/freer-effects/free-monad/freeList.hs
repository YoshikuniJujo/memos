{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data FreeList a
	= PureL a
	| JoinL [FreeList a]
	deriving Show

mulAdd :: Integer -> Integer -> [Integer]
mulAdd y x = [x + y, x * y]

instance Functor FreeList where
	f `fmap` PureL x = PureL $ f x
	f `fmap` JoinL m = JoinL $ fmap f `map` m

instance Applicative FreeList where
	pure = PureL
	PureL f <*> mx = f <$> mx
	JoinL fs <*> mx = JoinL $ (<*> mx) `map` fs

instance Monad FreeList where
	PureL x >>= f = f x
	JoinL xs >>= f = JoinL $ (f =<<) `map` xs

mulAddF :: Integer -> Integer -> FreeList Integer
mulAddF y x = JoinL [PureL $ x + y, PureL $ x * y]

runList :: FreeList a -> [a]
runList (PureL x) = [x]
runList (JoinL xs) = runList `concatMap` xs
