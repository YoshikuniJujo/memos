{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid

data Free t a
	= Pure a
	| Join (t (Free t a))

instance Functor t => Functor (Free t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` Join tx = Join $ fmap f <$> tx

instance Functor t => Applicative (Free t) where
	pure = Pure
	Pure f <*> m = f <$> m
	Join tf <*> m = Join $ (<*> m) <$> tf

instance Functor t => Monad (Free t) where
	Pure x >>= f = f x
	Join tx >>= f = Join $ (f =<<) <$> tx

data RW e w a
	= Reader (e -> a)
	| Writer w a

instance Functor (RW e w) where
	f `fmap` Reader k = Reader $ f . k
	f `fmap` Writer w x = Writer w $ f x

ask :: Free (RW e w) e
ask = Join $ Reader Pure

tell :: w -> Free (RW e w) ()
tell w = Join . Writer w $ Pure ()

runRW :: Monoid w => Free (RW e w) a -> e -> (a, w)
runRW m e = case m of
	Pure x -> (x, mempty)
	Join (Reader k) -> runRW (k e) e
	Join (Writer w m') -> second (w <>) $ runRW m' e

sample :: Free (RW String String) (String, String)
sample = do
	x <- ask
	tell $ "I say " ++ x ++ ".\n"
	y <- ask
	tell $ "You say Good-bye!\n"
	return (x, y)

runStateRW :: Free (RW s s) a -> s -> (a, s)
runStateRW m s = case m of
	Pure x -> (x, s)
	Join (Reader k) -> runStateRW (k s) s
	Join (Writer s' m') -> runStateRW m' s'
