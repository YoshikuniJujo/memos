{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data F
data T

type family Foo a
type instance Foo F = Double
type instance Foo T = Integer

class Bar b where
	data Baz b
	data Qux b
	bar :: Baz b -> Qux b
	showQux :: Qux b -> String

instance Bar F where
	data Baz F = BazF Double
	data Qux F = QuxF Integer
	bar (BazF x) = QuxF $ round x
	showQux (QuxF x) = "QuxF " ++ show x

instance Bar T where
	data Baz T = BazT Integer
	data Qux T = QuxT Double
	bar (BazT n) = QuxT $ fromInteger n
	showQux (QuxT n) = "QuxT " ++ show n

ex1 :: [Baz F]
ex1 = map BazF [4.3, 3.2, 2.1]

ex2 :: [Baz T]
ex2 = map BazT [43, 32, 21]

{-
class Bad b where
	type Ba b
	bad :: Ba b -> Ba b
	-}
