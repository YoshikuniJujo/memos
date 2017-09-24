{-# LANGUAGE ExistentialQuantification, TypeFamilies, GADTs, KindSignatures #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infixr 5 :-

data Empty
data NonEmpty

data family SafeList a b
data instance SafeList a Empty = Nil
data instance SafeList a NonEmpty = forall b . a :- SafeList a b

class SafeHeadable b where
	safeHead :: SafeList a b -> a

instance SafeHeadable NonEmpty where
	safeHead (x :- _) = x

-- infix 5 :=

data NotSafe
data Safe

{-
data family MarkedList :: * -> * -> *
data instance MarkedList a NotSafe = N
data instance MarkedList a c = forall b . a := MarkedList a b
-}

class MarkedHeadable b where
	data MarkedList a b
	markedHead :: MarkedList a b -> a

instance MarkedHeadable NotSafe where
	data MarkedList a NotSafe = N

instance MarkedHeadable Safe where
	data MarkedHeadable a Safe = 

{-
data MarkedList :: * -> * -> * where
	N :: MarkedList t NotSafe
	(:=) :: a -> MarkedList a b -> MarkedList a c

safeHeadM :: MarkedList a Safe -> a
safeHeadM (x := _) = x

silly False = N
silly True = () := N

-}
