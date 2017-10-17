data Product a b = Product a b deriving Show
data Sum a b = L a | R b deriving Show

x :: Sum (Product Char Bool) (Product String Integer)
x = L $ Product 'c' True
