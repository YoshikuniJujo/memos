{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Free t a
	= Pure a
	| Join (t (Free t a))

join' :: Functor t => Free t (Free t a) -> Free t a
join' (Pure m) = m
join' (Join tm) = Join $ join' <$> tm

removePure :: Functor t => Free t (Free t a) -> Free t a
removePure (Pure m) = m
removePure (Join tm) = Join $ removePure <$> tm

fmap' :: Functor t => (a -> b) -> Free t a -> Free t b
fmap' f (Pure x) = Pure $ f x
fmap' f (Join tx) = Join $ fmap' f <$> tx

jn :: Monad m => m (m a) -> m a
jn mmx = mmx >>= id

instance Functor t => Functor (Free t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` Join tx = Join $ (f <$>) <$> tx

instance Functor t => Applicative (Free t) where
	pure = Pure
	Pure f <*> m = f <$> m
	Join tf <*> m = Join $ (<*> m) <$> tf

instance Functor t => Monad (Free t) where
	Pure x >>= f = f x
	Join tx >>= f = Join $ (f =<<) <$> tx

data FreeList a
	= PureL a
	| JoinL [FreeList a]
	deriving Show

removePureL :: FreeList (FreeList a) -> FreeList a
removePureL (PureL m) = m
removePureL (JoinL tm) = JoinL $ removePureL <$> tm

instance Functor FreeList where
	f `fmap` PureL x = PureL $ f x
	f `fmap` JoinL tx = JoinL $ fmap f <$> tx

instance Applicative FreeList where
	pure = PureL
	PureL f <*> m = f <$> m
	JoinL tf <*> m = JoinL $ (<*> m) <$> tf

instance Monad FreeList where
	PureL x >>= f = f x
	JoinL tx >>= f = JoinL $ (f =<<) <$> tx

fromList :: [a] -> FreeList a
fromList = JoinL . fmap PureL

data FreeMaybe a
	= PureM a
	| JoinM [FreeMaybe a]

instance Functor FreeMaybe where
	f `fmap` PureM x = PureM $ f x
	f `fmap` JoinM tx = JoinM $ fmap f <$> tx

instance Applicative FreeMaybe where
	pure = PureM
	PureM f <*> m = f <$> m
	JoinM tf <*> m = JoinM $ (<*> m) <$> tf

instance Monad FreeMaybe where
	PureM x >>= f = f x
	JoinM tx >>= f = JoinM $ (f =<<) <$> tx

exampleIO :: Int -> IO Int
exampleIO n = putStrLn ("n = " ++ show n) >> return (n + 1)

replicate1Bool :: Bool -> [] Bool
replicate1Bool = replicate 1

replicate2Bool :: Bool -> [] Bool
replicate2Bool = replicate 2

replicate3Bool :: Bool -> [] Bool
replicate3Bool = replicate 3

toFree1 :: Functor t => t a -> Free t a
toFree1 = Join . fmap Pure

toFree2 :: Functor t => t (t a) -> Free t a
toFree2 = removePure . fmap toFree1 . toFree1

toFree3 :: Functor t => t (t ( t a)) -> Free t a
toFree3 = removePure . fmap toFree1 . toFree2

toFreeL1 :: [a] -> FreeList a
toFreeL1 = JoinL . fmap PureL

toFreeL2 :: [[a]] -> FreeList a
toFreeL2 = removePureL . fmap toFreeL1 . toFreeL1

toFreeL3 :: [[[a]]] -> FreeList a
toFreeL3 = removePureL . fmap toFreeL1 . toFreeL2

runIO :: Free IO a -> IO a
runIO (Pure x) = return x
runIO (Join act) = act >>= runIO

runMonad :: Monad m => Free m a -> m a
runMonad (Pure x) = return x
runMonad (Join m) = m >>= runMonad
