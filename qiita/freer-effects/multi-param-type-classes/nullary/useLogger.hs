{-# LANGUAGE NullaryTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Logger

instance Logger where
	logMessage t = putStrLn $ "[XMAS LOG]: " ++ t
