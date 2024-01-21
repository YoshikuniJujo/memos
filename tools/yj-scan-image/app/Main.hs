{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Time
import System.Environment
import System.Process
import System.Directory
import System.FilePath

main :: IO ()
main = do
	hm <- getEnv "HOME"
	tm <- tr ' ' '_' . show <$> getZonedTime
	let	dr = hm </> ".yj-scan-image"
		scnr = dr </> "scanner"
		imgdr = hm </> "scanned_image"
		pnm = imgdr </> "img_" ++ tm ++ ".pnm"
		png = imgdr </> "img_" ++ tm ++ ".png"
	createDirectoryIfMissing False dr
	createDirectoryIfMissing False imgdr
	e <- doesFileExist scnr
	when (not e) do
		rtn <- disass <$> readProcess "scanimage" ["--list-devices"] ""
		case rtn of
			Nothing -> error "no scanner"
			Just (_, dvc, _) -> writeFile scnr dvc
	snm <- readFile scnr
	putStrLn pnm
	putStrLn png
--	uncurry callProcess $ command snm pnm
	callCommand $ command' snm pnm
	callCommand $ "convert " ++ pnm ++ " " ++ png
	callCommand $ "rm " ++ pnm

disass :: String -> Maybe (String, String, String)
disass src = case span (/= '`') src of
	(a, '`' : b) -> case span (/= '\'') b of
		(c, '\'' : d) -> Just (a, c, d)
		_ -> Nothing
	_ -> Nothing

tr :: Char -> Char -> String -> String
tr s d = \case
	"" -> ""
	c : cs	| c == s -> d : tr s d cs
		| otherwise -> c : tr s d cs

command' scnr img = let (cmd, args) = command scnr img in cmd ++ " " ++ unwords args

command :: String -> String -> (FilePath, [String])
command scnr img = ("scanimage", [
	"--device-name='" ++ scnr ++ "'", 
	"--format=pnm",
	"--output-file=" ++ img ])
