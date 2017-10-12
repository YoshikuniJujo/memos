{-# LANGUAGE ExistentialQuantification, KindSignatures, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Union0 a = forall t . Union0 (t a)

data Union1 (ts :: [* -> *]) a = forall t . Union !Word (t a)
