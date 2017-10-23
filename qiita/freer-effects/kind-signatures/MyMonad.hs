{-# LANGUAGE KindSignatures #-}

module MyMonad (fun) where

class MyMonad (m :: * -> *)

fun :: (Monad m, MyMonad m) => m a -> m b -> m b
fun = (>>)

instance MyMonad []
instance MyMonad IO
