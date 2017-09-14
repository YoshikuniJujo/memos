{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data SomeShape = forall s . Shape s => SomeShape s

area :: SomeShape -> Double
area (SomeShape s) = shapeArea s

class Shape s where
	shapeArea :: s -> Double

data Rectangle = Rectangle (Double, Double) Double Double

instance Shape Rectangle where
	shapeArea (Rectangle _ w h) = w * h

data Circle = Circle (Double, Double) Double

instance Shape Circle where
	shapeArea (Circle _ r) = r * r * pi

areas :: [SomeShape] -> [Double]
areas = map area
