{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infixr 9 :>>>

data FunList a b
	= Fun (a -> b)
	| forall x . Endable x => (a -> x) :>>> FunList x b

class Endable e where end :: e -> Bool

instance Endable Integer where
	end = \case 0 -> True; _ -> False

instance Endable [a] where
	end = \case [] -> True; _ -> False

applyAll :: FunList a b -> a -> b
applyAll (Fun f) = f
applyAll (f :>>> fs) = applyAll fs . f

notEnd :: Endable a => FunList a b -> a -> Maybe b
notEnd _ x | end x = Nothing
notEnd (Fun f) x = Just $ f x
notEnd (f :>>> fs) x = notEnd fs $ f x

sample :: FunList Integer String
sample = subtract 8 :>>> (+ 5) :>>> (* 3) :>>> show :>>> Fun reverse
