{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Free
import Coyoneda

data Reader e a where
	Reader :: Reader e e

runReaderCoyoneda :: Coyoneda (Reader e) a -> e -> a
runReaderCoyoneda (Coyoneda Reader k) e = k e

ask :: Free (Coyoneda (Reader e)) e
ask = Join $ Pure <$> coyoneda Reader

runReader :: Free (Coyoneda (Reader e)) a -> e -> a
runReader m e = case m of
	Pure x -> x
	Join (Coyoneda Reader k) -> runReader (k e) e
