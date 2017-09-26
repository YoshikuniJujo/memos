{-# LANGUAGE GADTSyntax #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infix 4 :=:
infixl 6 :+:
infixl 7 :*:

data ExprInt where
	I :: Int -> ExprInt
	(:+:) :: ExprInt -> ExprInt -> ExprInt
	(:*:) :: ExprInt -> ExprInt -> ExprInt

data ExprBool where
	B :: Bool -> ExprBool
	(:=:) :: ExprInt -> ExprInt -> ExprBool

evalInt :: ExprInt -> Int
evalInt (I n) = n
evalInt (e1 :+: e2) = evalInt e1 + evalInt e2
evalInt (e1 :*: e2) = evalInt e1 * evalInt e2

evalBool :: ExprBool -> Bool
evalBool (B b) = b
evalBool (e1 :=: e2) = evalInt e1 == evalInt e2
