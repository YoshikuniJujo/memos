{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Automaton a where
	data State a
	initialState :: State a
	doesAccept :: State a -> Bool
	transition :: State a -> a -> Maybe (State a)

data Input = Zero | One deriving Show

instance Automaton Input where
	data State Input = Q0 | Q1
	initialState = Q0
	doesAccept = \case Q0 -> True; Q1 -> False
	transition s i = Just $ case (s, i) of
		(Q0, Zero) -> Q1
		(Q0, One) -> Q0
		(Q1, Zero) -> Q0
		(Q1, One) -> Q1

check :: Automaton a => [a] -> Bool
check = maybe False doesAccept . checkFrom initialState

checkFrom :: Automaton a => State a -> [a] -> Maybe (State a)
checkFrom s [] = Just s
checkFrom s (i : is) = do
	s' <- transition s i
	checkFrom s' is
