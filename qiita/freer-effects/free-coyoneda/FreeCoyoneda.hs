{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreeCoyoneda where

import Control.Arrow
import Data.Monoid

import Free
import Coyoneda

type FreeCoyoneda t = Free (Coyoneda t)

data Reader e a where
	Reader :: Reader e e

ask :: FreeCoyoneda (Reader e) e
ask = Join $ Pure <$> coyoneda Reader

runReader :: FreeCoyoneda (Reader e) a -> e -> a
runReader m e = case m of
	Pure x -> x
	Join (Coyoneda Reader k) -> runReader (k e) e

data Writer w a where
	Writer :: w -> Writer w ()

tell :: w -> FreeCoyoneda (Writer w) ()
tell = Join . (Pure <$>) . coyoneda . Writer

runWriter :: Monoid w => FreeCoyoneda (Writer w) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Join (Coyoneda (Writer w) k) -> second (w <>) . runWriter $ k ()

data RW e w a where
	R :: RW e w e
	W :: w -> RW e w ()

ask' :: FreeCoyoneda (RW e w) e
ask' = Join $ Pure <$> coyoneda R

tell' :: w -> FreeCoyoneda (RW e w) ()
tell' = Join . (Pure <$>) . coyoneda . W

sample :: FreeCoyoneda (RW String String) (String, String)
sample = do
	x <- ask'
	tell' $ "I say " ++ x ++ ".\n"
	y <- ask'
	tell' $ "You say Good-bye!\n"
	return (x, y)

runRW :: Monoid w => FreeCoyoneda (RW e w) a -> e -> (a, w)
runRW m e = case m of
	Pure x -> (x, mempty)
	Join (Coyoneda R k) -> runRW (k e) e
	Join (Coyoneda (W w) k) -> second (w <>) $ runRW (k ()) e

runStateRW :: FreeCoyoneda (RW s s) a -> s -> (a, s)
runStateRW m s = case m of
	Pure x -> (x, s)
	Join (Coyoneda R k) -> runStateRW (k s) s
	Join (Coyoneda (W s') k) -> runStateRW (k ()) s'
