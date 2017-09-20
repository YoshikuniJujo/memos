{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Bool
import Text.XML.YJSVG (showSVG)
import Graphics.X11.Turtle
import System.Environment

size :: (Double, Double)
size = (450, 300)

width, height :: Double
width = fst size
height = snd size

main :: IO ()
main = do
	args <- getArgs

	f <- openField
	g <- newTurtle f
	penup g
	hideturtle g
	pencolor g "gray"
	t <- newTurtle f
	penup t
	hideturtle t

	drawIt g (`backward` (width / 2))
	home g
	drawIt g (`forward` (width / 2))
	setheading g 90
	goto g 0 0
	drawIt g (`backward` (height / 2))
	goto g 0 0
	drawIt g (`forward` (height / 2))

	topleft f

	goto t (width / 2) (height / 2)
	backward t 100
	setheading t 90
	backward t 30
	setheading t 0
	drawIt t (`circle` 30)
	setheading t 90
	backward t 5
	setheading t 0
	drawIt t (`circle` 35)
	setheading t 90
	forward t 25
	setheading t 0
	backward t 10
	write t "KochiGothic" 24 "S1"

	goto t (width / 2) (height / 2)
	setheading t 0
	forward t 100
	setheading t 90
	backward t 35
	setheading t 0
	drawIt t (`circle` 35)
	setheading t 90
	forward t 25
	setheading t 0
	backward t 10
	write t "KochiGothic" 24 "S2"

	goto t (width / 2) (height / 2)
	setheading t 0
	backward t 190
	pensize t 3
	drawIt t (`forward` 45)
	left t 30
	drawIt t (`backward` 15)
	forward t 15
	right t 60
	drawIt t (`backward` 15)

	grd <- getSVG g
	svg <- getSVG t
	writeFile "./svgs/automaton.svg" . showSVG width height . map ("" ,)
		$ bool id ((grd ++) . tail) (args == ["grid"]) svg
	putStr . showSVG 300 200 $ map ("" ,) grd

drawIt :: Turtle -> (Turtle -> IO ()) -> IO ()
drawIt t act = pendown t >> act t >> penup t
