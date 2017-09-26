{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infix 4 :=:
infixl 6 :+:
infixl 7 :*:

data Expr a where
	I :: Int -> Expr Int
	B :: Bool -> Expr Bool
	(:+:) :: Expr Int -> Expr Int -> Expr Int
	(:*:) :: Expr Int -> Expr Int -> Expr Int
	(:=:) :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (e1 :=: e2) = eval e1 == eval e2

{-
make :: a -> Expr a
make n = I n
make b = B b
-}
