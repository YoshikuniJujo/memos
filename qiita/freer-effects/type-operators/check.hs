{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- type a + b = (a, b)

-- data a + b = a :+ b
-- data a :+: b = a :+: b

-- data Product a b = a :*: b
-- data Sum a b = L a | R b
-- some :: Sum (Product Char Bool) (String Integer)
-- some = L $ 'c' :*: True

infixl 7 *
infixl 6 +

data a * b = a :*: b deriving Show
data a + b = L a | R b deriving Show

some :: Char * Bool + String * Integer
some = L $ 'c' :*: True
