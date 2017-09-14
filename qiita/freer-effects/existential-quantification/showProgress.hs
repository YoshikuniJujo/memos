{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow

infixr 9 :.:
data FunList a b
	= Fun (a -> b)
	| forall x . Show x => (x -> b) :.: FunList a x

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :.: fs) = f . apply fs

sample :: FunList Integer String
sample = reverse :.: show :.: (+ 5) :.: Fun (* 10)

showProgress :: Show a => FunList a b -> a -> (b, [String])
showProgress (Fun f) = (f &&& (:[]) . show)
showProgress (f :.: fs) =
	(f . fst &&& uncurry (:) . first show) . showProgress fs

{-
(f y, show y : ps)
	where (y, ps) = showProgress fs x
	-}

{-
infixr 9 :>>>
data Fun a b = Fun (a -> b) | forall x . Show x => (a -> x) :>>> Fun x b

apply :: Fun a b -> a -> b
apply (Fun f) = f
apply (f :>>> fs) = apply fs . f

sample :: Fun Integer String
sample = (* 10) :>>> (+ 5) :>>> show :>>> Fun reverse

showProgress :: Fun a b -> a -> (b, [String])
showProgress (Fun f) x = (
-}
