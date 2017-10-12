{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Eff

data Reader e a where
	Reader :: Reader e e

ask :: Eff e
ask = send Reader

runReader :: Eff a -> e -> Eff a
runReader m e = case m of
	Pure x -> return x
	u `Bind` k -> 
