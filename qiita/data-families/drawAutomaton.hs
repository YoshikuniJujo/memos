{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad
import Data.Bool
import Text.XML.YJSVG (showSVG)
import Graphics.X11.Turtle
import System.Environment

size :: (Double, Double)
size = (360, 170)

width, height :: Double
width = fst size
height = snd size

cWidth, cHeight :: Double
cWidth = width * 9 / 16
cHeight = height * 11 / 16

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

	goto t cWidth cHeight
	setheading t 0
	backward t 190
	pensize t 3
	drawIt t (`forward` 45)
	left t 30
	drawIt t (`backward` 15)
	forward t 15
	right t 60
	drawIt t (`backward` 15)
	setheading t 0
	pensize t 1

	goto t cWidth cHeight
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

	goto t cWidth cHeight
	backward t 80
	setheading t 90
	forward t 45
	arrow1 t
	setheading t 0
	pensize t 1

	goto t cWidth cHeight
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

	goto t cWidth cHeight
	forward t 120
	setheading t 90
	forward t 45
	arrow1 t
	setheading t 0
	pensize t 1

	goto t cWidth cHeight
	setheading t 0
	backward t 50
	setheading t 90
	forward t 20
	setheading t 30
	arrow2 t

	setheading t 0
	forward t 5
	setheading t 90
	backward t 40
	setheading t (- 150)
	arrow2 t

	goto t cWidth cHeight
	setheading t 90
	backward t 50
	write t "KochiGothic" 18 "0"
	forward t 100
	write t "KochiGothic" 18 "0"

	grd <- getSVG g
	svg <- getSVG t
	writeFile "./svgs/automaton.svg" . showSVG width height . map ("" ,)
		$ bool id ((grd ++) . tail) (args == ["grid"]) svg
	putStr . showSVG 300 200 $ map ("" ,) grd

drawIt :: Turtle -> (Turtle -> IO ()) -> IO ()
drawIt t act = pendown t >> act t >> penup t

arrow1 :: Turtle -> IO ()
arrow1 t = do
	p <- position t
	pensize t 3
	setheading t 60
	drawIt t $ \s -> do
		replicateM_ 25 $ forward s 4 >> left t 10
		forward s 4
	left t 15
	drawIt t (`backward` 15)
	forward t 15
	right t 60
	drawIt t (`backward` 15)
	uncurry (goto t) p
	setheading t 90
	forward t 50
	setheading t 0
	backward t 24
	write t "KochiGothic" 18 "1"

arrow2 :: Turtle -> IO ()
arrow2 t = do
	pensize t 3
	drawIt t $ \s -> do
		replicateM_ 7 $ forward s 15 >> right t 10
		forward s 5
	left t 30
	drawIt t (`backward` 15)
	forward t 15
	right t 60
	drawIt t (`backward` 15)
	pensize t 1
