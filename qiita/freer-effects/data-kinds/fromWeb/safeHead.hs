{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head, tail)
import GHC.Exts

data Z
data S n
data Vector n a where
	Nil :: Vector Z a
	(:-) :: a -> Vector n a -> Vector (S n) a

instance Show a => Show (Vector n a) where
	show Nil = "Nil"
	show (x :- xs) = show x ++ " :- " ++ show xs

instance IsList (Vector Z a) where
	type Item (Vector Z a) = a
	fromList [] = Nil
	fromList _ = error "bad"
	toList Nil = []

instance (IsList (Vector n a), Item (Vector n a) ~ a) =>
	IsList (Vector (S n) a) where
	type Item (Vector (S n) a) = a
	fromList [] = error "bad"
	fromList (x : xs) = x :- fromList xs
	toList (x :- xs) = x : toList xs

head :: Vector (S n) a -> a
head (x :- _) = x

tail :: Vector (S n) a -> Vector n a
tail (_ :- xs) = xs

sample :: Vector (S (S (S Z))) Integer
sample = [3, 4, 5]
