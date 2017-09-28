{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Concurrent
import Data.Maybe
import System.IO.Unsafe

import Coyoneda

slowSucc :: Integer -> Integer
slowSucc n = unsafePerformIO $ threadDelay 1000000 >> return (succ n)

data StrictMaybe a = StrictNothing | StrictJust !a deriving Show

isStrictJust :: StrictMaybe a -> Bool
isStrictJust (StrictJust _) = True
isStrictJust StrictNothing = False

instance Functor StrictMaybe where
	fmap _ StrictNothing = StrictNothing
	fmap f (StrictJust x) = StrictJust $ f x

headIfAllJust :: [StrictMaybe a] -> Maybe a
headIfAllJust (StrictJust x : ms)
	| all isStrictJust ms = Just x
headIfAllJust _ = Nothing

isStrictJustCoyoneda :: Coyoneda StrictMaybe a -> Bool
isStrictJustCoyoneda (Coyoneda (StrictJust _) _) = True
isStrictJustCoyoneda (Coyoneda StrictNothing _) = False

headIfAllJustCoyoneda :: [Coyoneda StrictMaybe a] -> Coyoneda Maybe a
headIfAllJustCoyoneda (Coyoneda (StrictJust x) f : ms)
	| all isStrictJustCoyoneda ms = Coyoneda (Just x) f
headIfAllJustCoyoneda _ = coyoneda Nothing
