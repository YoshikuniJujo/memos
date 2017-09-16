{-# LANGUAGE TypeFamilies, DataKinds, TypeFamilyDependencies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Nat = Zero | Succ Nat deriving Show

class DeepHead (n :: Nat) where
	type List n = l | l -> n
	deepHead :: List n -> Int

instance DeepHead 'Zero where
	type List 'Zero = Int
	deepHead = id
