{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Foo x

type instance Foo Integer = Bool
type instance Foo Double = Char
