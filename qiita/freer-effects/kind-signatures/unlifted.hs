{-# LANGUAGE MagicHash #-}

import GHC.Types
import GHC.Prim

n = I# (8# :: Int#)

getI (I# n) = n

apply f x = f x
