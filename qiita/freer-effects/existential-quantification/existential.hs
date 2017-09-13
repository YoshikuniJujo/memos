{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce
import GHC.Types

data Foo = forall x . Foo x

fromFoo :: Foo -> x
fromFoo (Foo x) = unsafeCoerce x

data Showable = forall x . Show x => Showable x

instance Show Showable where
	show (Showable x) = "Showable " ++ show x

data Fun a b = forall x . Fun (a -> x) (x -> b)

compose :: (a -> b) -> (b -> c) -> Fun a c
compose g f = Fun g f

fun :: Fun a b -> a -> b
fun (Fun g f) = f . g

data Typed = forall x . Typed String x

fromInt :: Int -> Typed
fromInt = Typed "Int"

fromBool :: Bool -> Typed
fromBool = Typed "Bool"

fromChar :: Char -> Typed
fromChar = Typed "Char"

toInt :: Typed -> Maybe Int
toInt (Typed "Int" x) = Just $ unsafeCoerce x
toInt _ = Nothing

toBool :: Typed -> Maybe Bool
toBool (Typed "Bool" x) = Just $ unsafeCoerce x
toBool _ = Nothing

toChar :: Typed -> Maybe Char
toChar (Typed "Char" x) = Just $ unsafeCoerce x
toChar _ = Nothing

data SomeShape = forall s . Shape s => SomeShape s

data ClosedShape
	= ShapeRectangle Rectangle
	| ShapeCircle Circle

class Shape s where
	area :: s -> Double

data Rectangle = Rectangle {
	topleft :: (Double, Double),
	width :: Double,
	height :: Double } deriving Show

instance Shape Rectangle where
	area r = width r * height r

sampleRectangle :: Rectangle
sampleRectangle = Rectangle (2, 5) 3 9

data Circle = Circle {
	center :: (Double, Double),
	radius :: Double } deriving Show

instance Shape Circle where
	area c = radius c ^ (2 :: Int) * pi

sampleCircle :: Circle
sampleCircle = Circle (- 5, 11) 8

areas :: [SomeShape] -> [Double]
areas (SomeShape s : ss) = area s : areas ss
areas [] = []

areasC :: [ClosedShape] -> [Double]
areasC (ShapeRectangle r : ss) = area r : areasC ss
areasC (ShapeCircle c : ss) = area c : areasC ss
areasC [] = []
