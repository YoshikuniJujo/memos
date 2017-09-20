{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class DFA alphabet where
	data States alphabet
	initialState :: States alphabet
	accept :: States alphabet -> Bool
	transition :: States alphabet -> alphabet -> Maybe (States alphabet)

check :: DFA alphabet => [alphabet] -> Bool
check = maybe False accept . run initialState

run :: DFA alphabet =>
	States alphabet -> [alphabet] -> Maybe (States alphabet)
run s [] = Just s
run s (a : as) = do
	s' <- transition s a
	run s' as

data ExampleAlphabet = Zero | One deriving Show

instance DFA ExampleAlphabet where
	data States ExampleAlphabet = S1 | S2
	initialState = S1
	accept = \case S1 -> True; S2 -> False
	transition s a = Just $ case (s, a) of
		(S1, Zero) -> S2
		(S1, One) -> S1
		(S2, Zero) -> S1
		(S2, One) -> S2
