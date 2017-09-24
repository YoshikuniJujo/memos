{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce

infix 4 :=:
infixl 6 :+:
infixl 7 :*:

data Expr a
	= I Int
	| B Bool
	| Expr a :+: Expr a
	| Expr a :*: Expr a
	| Expr a :=: Expr a
	deriving Show

eval :: Expr a -> a
eval (I n) = unsafeCoerce n
-- eval (e1 :+: e2) =
--	(unsafeCoerce $ eval e1 :: Int) + (unsafeCoerce $ eval e2 :: Int)
-- eval (e1 :*: e2) = eval e1 * eval e2

(.+.) :: Expr Int -> Expr Int -> Expr Int
(.+.) = (:+:)

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B
