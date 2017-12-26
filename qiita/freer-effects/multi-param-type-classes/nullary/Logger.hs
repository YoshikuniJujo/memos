{-# LANGUAGE NullaryTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Logger where

class Logger where
	logMessage :: String -> IO ()

type Present = String

queueNewChrismasPresents :: Logger => [Present] -> IO ()
queueNewChrismasPresents presents =
	mapM_ (logMessage . ("Queueing present for delivery: " ++)) presents
