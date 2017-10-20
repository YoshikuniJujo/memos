{-# LANGUAGE KindSignatures #-}

type Foo t = t Integer
type Bar t u v = t u v

type Baz t (u :: * -> * -> *) v = t u v

type Hoge t u = t u

type Some t u = Int

class FooClass a where

class BarClass (a :: * -> *) where

instance FooClass Int where

-- instance FooClass Maybe where

instance BarClass Maybe where

val :: BarClass m => m a -> m a
val = id
