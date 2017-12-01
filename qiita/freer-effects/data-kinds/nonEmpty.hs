{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Exts

data List a = Empty | NE (NonEmpty a)

instance IsList (List a) where
	type Item (List a) = a
	fromList [] = Empty
	fromList xs = NE $ fromList xs
	toList Empty = []
	toList (NE xs) = toList xs

instance Show a => Show (List a) where
	show = show . toList

instance Foldable List where
	foldr _ v Empty = v
	foldr op v (NE xs) = foldr op v xs

instance Functor List where
	fmap _ Empty = Empty
	fmap f (NE xs) = NE $ fmap f xs

data NonEmpty a = a :! List a

instance IsList (NonEmpty a) where
	type Item (NonEmpty a) = a
	fromList [] = error "bad"
	fromList (x : xs) = x :! fromList xs
	toList (x :! xs) = x : toList xs

instance Show a => Show (NonEmpty a) where
	show = show . toList

instance Foldable NonEmpty where
	foldr op v (x :! xs) = x `op` foldr op v xs

instance Functor NonEmpty where
	fmap f (x :! xs) = f x :! fmap f xs

headNE :: NonEmpty a -> a
headNE (x :! _) = x

tailNE :: NonEmpty a -> List a
tailNE (_ :! xs) = xs
