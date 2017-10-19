{-# LANGUAGE KindSignatures #-}

type Foo t = t Integer
type Bar t u v = t u v

type Baz t (u :: * -> * -> *) v = t u v

type Hoge t u = t u
