{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Bool
import Text.XML.YJSVG (showSVG)
import Graphics.X11.Turtle
import System.Environment

main :: IO ()
main = do
	args <- getArgs

	f <- openField
	g <- newTurtle f
	penup g
	pencolor g "gray"
	t <- newTurtle f
	penup t

	drawIt g (`backward` 150)
	home g
	drawIt g (`forward` 150)
	setheading g 90
	goto g 0 0
	drawIt g (`backward` 100)
	goto g 0 0
	drawIt g (`forward` 100)

	topleft f
	goto t 150 100
	drawIt t (`forward` 100)

	grd <- getSVG g
	svg <- getSVG t
	writeFile "./svgs/automaton.svg" . showSVG 300 200 . map ("" ,)
		$ bool id ((grd ++) . tail) (args == ["grid"]) svg
	putStr . showSVG 300 200 $ map ("" ,) grd

drawIt :: Turtle -> (Turtle -> IO ()) -> IO ()
drawIt t act = pendown t >> act t >> penup t
