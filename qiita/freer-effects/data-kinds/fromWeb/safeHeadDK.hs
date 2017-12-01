{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Exts

data Nat = Z | S Nat

data Vector n a where
	Nil :: Vector 'Z a
	(:-) :: a -> Vector n a -> Vector ('S n) a

infixr 5 :-

instance Show a => Show (Vector n a) where
	show Nil = "Nil"
	show (x :- xs) = show x ++ " :- " ++ show xs

instance IsList (Vector 'Z a) where
	type Item (Vector 'Z a) = a
	fromList [] = Nil
	fromList _ = error "bad"
	toList Nil = []

instance (IsList (Vector n a), Item (Vector n a) ~ a) =>
	IsList (Vector ('S n) a) where
	type Item (Vector ('S n) a) = a
	fromList [] = error "bad"
	fromList (x : xs) = x :- fromList xs
	toList (x :- xs) = x : toList xs

-- instance Num Nat where
