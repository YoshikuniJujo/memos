{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data SomeShape = forall s . Shape s => SomeShape s

class Shape s where
	area :: s -> Double

instance Shape SomeShape where
	area (SomeShape s) = area s

data Rectangle = Rectangle (Double, Double) Double Double

instance Shape Rectangle where
	area (Rectangle _ w h) = w * h

data Circle = Circle (Double, Double) Double

instance Shape Circle where
	area (Circle _ r) = r * r * pi

areas :: [SomeShape] -> [Double]
areas = map area
