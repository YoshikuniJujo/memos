{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infix 4 :=:
infixl 6 :+:
infixl 7 :*:

class Evalable a where
	data Expr a
	eval :: Expr a -> a

instance Evalable Int where
	data Expr Int
		= I Int
		| Expr Int :+: Expr Int
		| Expr Int :*: Expr Int
	eval (I n) = n
	eval (e1 :+: e2) = eval e1 + eval e2
	eval (e1 :*: e2) = eval e1 * eval e2

instance Evalable Bool where
	data Expr Bool
		= B Bool
		| Expr Int :=: Expr Int
	eval (B b) = b
	eval (e1 :=: e2) = eval e1 == eval e2
