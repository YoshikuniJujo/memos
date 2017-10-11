{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnion where

data Union a = forall t . Union (t a)

inj :: t a -> Union a
inj = Union
