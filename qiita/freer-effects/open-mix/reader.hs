{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Eff

data Reader e a where
	Reader :: Reader e e
