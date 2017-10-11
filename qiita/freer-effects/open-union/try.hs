{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce

data Value
	= Unit ()
	| Bool Bool
	| Integer Integer
	| Double Double
	| Char Char
	deriving Show

data UnionValue = forall x . UnionValue x

getBool :: UnionValue -> Bool
getBool (UnionValue b) = unsafeCoerce b

class Coerceable a b where
	coerce :: a -> b

instance Coerceable a a where
	coerce = id

instance {-# Overlappable #-} Coerceable a b where
	coerce = error "can't coerce"

data Union a = forall t . Union (t a)

data SE s e a where
	Get :: SE s e s
	Put :: s -> SE s e ()
	Exc :: e -> SE s e a

data State s a where
	Get' :: State s s
	Put' :: s -> State s ()

instance Show s => Show (State s a) where
	show Get' = "Get'"
	show (Put' s) = "(Put' " ++ show s ++ ")"

data Exc e a where
	Exc' :: e -> Exc e a
	deriving Show

getState :: Union a -> State s a
getState (Union s) = unsafeCoerce s

getExc :: Union a -> Exc e a
getExc (Union e) = unsafeCoerce e
