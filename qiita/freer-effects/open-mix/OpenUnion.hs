{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnion (Union, inj, decomp) where

import Unsafe.Coerce (unsafeCoerce)

data Union a = forall t . Union (t a)

inj :: t a -> Union a
inj = Union

decomp :: Union a -> t a
decomp (Union tx) = unsafeCoerce tx
