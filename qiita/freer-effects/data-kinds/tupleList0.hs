{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

ex1 :: (Integer, (Double, (Bool, ())))
ex1 = (123, (3.14, (True, ())))

car :: (a, b) -> a
car = fst

cdr :: (a, b) -> b
cdr = snd
