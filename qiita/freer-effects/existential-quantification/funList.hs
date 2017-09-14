{-# LANGUAGE ExistentialQuantification #-}

{-
infixr 9 :>>>
data FunList a b = Fun (a -> b) | forall x . (a -> x) :>>> FunList x b

sample :: FunList Integer String
sample = (* 3) :>>> (+ 5) :>>> Fun show

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :>>> fs) = apply fs . f
-}

{-
notZero :: (Num a, Eq a) => FunList a b -> a -> Maybe b
notZero _ 0 = Nothing
notZero (Fun f) x = Just $ f x
notZero (f :>>> fs) x = notZero fs $ f x
-}

infixr 9 :.:
data FunList a b = Fun (a -> b) | forall x . (x -> b) :.: FunList a x

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :.: fs) = f . apply fs
