{-# LANGUAGE GADTs, KindSignatures #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infixr 5 :-

data Empty
data NonEmpty

data SafeList a b where
	Nil :: SafeList a Empty
	(:-) :: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (x :- _) = x

{-
silly False = Nil
silly True = () :- Nil
-}

infix 5 :=

data NotSafe
data Safe

data MarkedList :: * -> * -> * where
	N :: MarkedList t NotSafe
	(:=) :: a -> MarkedList a b -> MarkedList a c

safeHeadM :: MarkedList a Safe -> a
safeHeadM (x := _) = x

silly False = N
silly True = () := N
