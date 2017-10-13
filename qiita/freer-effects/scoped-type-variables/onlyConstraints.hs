{-# LANGUAGE ScopedTypeVariables #-}

class Foo f where

foo :: forall a . Foo a => String
foo = "hello" :: a
