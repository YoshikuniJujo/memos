{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Free
import Coyoneda

data Id a = Id a deriving Show

sample :: Free (Coyoneda Id) String
sample = do
	n <- return (123 :: Integer)
	s <- return "hello"
	return $ s ++ show n

runId :: Free (Coyoneda Id) a -> a
runId = \case
	Pure x -> x
	Join (Coyoneda (Id x) k) -> runId $ k x
