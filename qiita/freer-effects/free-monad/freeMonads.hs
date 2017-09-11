{-# LANGUAGE LambdaCase #-}

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

data Reader e a = Reader (e -> a)

instance Functor (Reader e) where
	f `fmap` Reader k = Reader $ f . k

ask :: Free (Reader e) e
ask = Join $ Reader Pure

runReader :: Free (Reader e) a -> e -> a
runReader m e = case m of
	Pure x -> x
	Join (Reader k) -> runReader (k e) e

sampleReader :: Free (Reader Char) Int
sampleReader = do
	c <- ask
	return $ fromEnum c

data Writer w a = Writer w a

instance Functor (Writer w) where
	f `fmap` Writer w x = Writer w $ f x

tell :: w -> Free (Writer w) ()
tell w = Join . Writer w $ Pure ()

runWriter :: Monoid w => Free (Writer w) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Join (Writer w m) -> second (w <>) $ runWriter m

sampleWriter :: Free (Writer String) Int
sampleWriter = do
	tell "3 * 2 = "
	tell $ show (3 * 2 :: Int)
	return $ 3 * 2

data State s a = State (s -> s) (s -> a)

instance Functor (State s) where
	f `fmap` State g k = State g $ f . k

getModify :: (s -> s) -> Free (State s) s
getModify g = Join $ State g Pure

get :: Free (State s) s
get = getModify id

modify :: (s -> s) -> Free (State s) ()
modify = (>> return ()) . getModify

put :: s -> Free (State s) ()
put = modify . const

runState :: Free (State s) a -> s -> (a, s)
runState m s = case m of
	Pure x -> (x, s)
	Join (State g k) -> runState (k s) (g s)

sampleState :: Free (State Integer) Integer
sampleState = do
	modify (+ 3)
	modify (* 8)
	n <- get
	modify (* 100)
	return n

data State2 s a = State2 (s -> (a, s))

getModify2 :: (s -> s) -> Free (State2 s) s
getModify2 g = Join . State2 $ \s -> (Pure s, g s)

runState2 :: Free (State2 s) a -> s -> (a, s)
runState2 m s = case m of
	Pure x -> (x, s)
	Join (State f) -> let (m', s') = f s in runState2 m' s'
