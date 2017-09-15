{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Nat = Zero | Succ Nat deriving Show

type family Foo x = f | f -> x where
	Foo 'Zero = Int
	Foo ('Succ n) = [Foo n]

class Bar (n :: Nat) where
	bar :: Foo n -> Int

instance Bar 'Zero where
	bar = id

-- instance {-# OVERLAPPABLE #-} Bar (Succ n) where
instance Bar n => Bar ('Succ n) where
--	bar = (bar :: Foo n -> Int) . head
	bar = bar . head
