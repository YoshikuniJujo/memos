{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data FreeIO a
	= PureIO a
	| JoinIO (IO (FreeIO a))

count :: Integer -> IO Integer
count n = putStrLn ("n = " ++ show n) >> return (n + 1)

instance Functor FreeIO where
	f `fmap` PureIO x = PureIO $ f x
	f `fmap` JoinIO m = JoinIO $ fmap f <$> m

instance Applicative FreeIO where
	pure = PureIO
	PureIO f <*> mx = f <$> mx
	JoinIO mf <*> mx = JoinIO $ (<*> mx) <$> mf

instance Monad FreeIO where
	PureIO x >>= f = f x
	JoinIO mx >>= f = JoinIO $ (f =<<) <$> mx

countF :: Integer -> FreeIO Integer
countF n = JoinIO $ putStrLn ("n = " ++ show n) >> return (PureIO $ n + 1)

runIO :: FreeIO a -> IO a
runIO = \case
	PureIO x -> return x
	JoinIO m -> m >>= runIO

runWith :: FreeIO a -> IO b -> IO a
runWith fio act = case fio of
	PureIO x -> return x
	JoinIO m -> act >> m >>= (`runWith` act)
