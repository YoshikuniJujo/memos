{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Concurrent
import System.IO.Unsafe

import Coyoneda

slowSucc :: Integer -> Integer
slowSucc n = unsafePerformIO $ threadDelay 1000000 >> return (succ n)

data StrictMaybe a = StrictNothing | StrictJust !a deriving Show

isStrictJust :: StrictMaybe a -> Bool
isStrictJust StrictNothing = False
isStrictJust (StrictJust _) = True

instance Functor StrictMaybe where
	fmap _ StrictNothing = StrictNothing
	fmap f (StrictJust x) = StrictJust $ f x

headIfAllJust :: [StrictMaybe a] -> StrictMaybe a
headIfAllJust mms@(m : _) | all isStrictJust mms = m
headIfAllJust _ = StrictNothing

headIfAllJustCoyoneda :: [Coyoneda StrictMaybe a] -> Coyoneda StrictMaybe a
headIfAllJustCoyoneda mms@(m : _) | all (fromContext isStrictJust) mms = m
headIfAllJustCoyoneda _ = coyoneda StrictNothing
