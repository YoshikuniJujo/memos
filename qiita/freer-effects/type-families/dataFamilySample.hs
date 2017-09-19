{-# LANGUAGE TypeFamilies #-}

data family Foo x

data instance Foo Int = FooInt Integer deriving Show
data instance Foo Double = FooDouble Bool deriving Show
