{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Time
import System.Environment
import System.Directory
import System.FilePath

main :: IO ()
main = do
	dr <- (</> ".try-hoge") <$> getEnv "HOME"
	createDirectoryIfMissing False dr
	writeFile (dr </> "current-time.txt")
		. (++ "\n") . show =<< getCurrentTime
