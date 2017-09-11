{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Mon a
	= Pure a
	| Join (Mon a)
	deriving Show

instance Functor Mon where
	f `fmap` Pure x = Pure $ f x
	f `fmap` Join m = Join $ f `fmap` m

instance Applicative Mon where
	pure = Pure
	Pure f <*> mx = f <$> mx
	Join mf <*> mx = mf <*> mx

instance Monad Mon where
	Pure x >>= f = f x
	Join m >>= f = m >>= f

some :: Integer -> Mon Integer
some n = Join . Pure $ n * 3

sample :: Mon Integer
sample = do
	x <- some 5
	y <- some $ 3 + x
	z <- some $ y - 3
	some $ z + 1

data Mon2 a
	= Pure2 a
	| Join2 (Mon2 (Mon2 a))
	deriving Show

instance Functor Mon2 where
	f `fmap` Pure2 x = Pure2 $ f x
	f `fmap` Join2 mm = Join2 $ fmap f <$> mm

instance Applicative Mon2 where
	pure = Pure2
	Pure2 f <*> mx = f <$> mx
	Join2 mmf <*> mx = Join2 $ (<*> mx) <$> mmf

instance Monad Mon2 where
	Pure2 x >>= f = f x
	Join2 mmx >>= f = Join2 . Join2 $ (f <$>) <$> mmx

other :: Integer -> Mon2 Integer
other n = Join2 . Pure2 . Pure2 $ n * 3

sample2 :: Mon2 Integer
sample2 = do
	x <- other 5
	y <- other $ 3 + x
	z <- other $ y - 3
	other $ z + 1

data BuildState s a
	= PureS a
	| State (s -> s) (s -> BuildState s a)

instance Functor (BuildState s) where
	f `fmap` PureS x = PureS $ f x
	f `fmap` State k x = State k $ (f <$>) . x

instance Applicative (BuildState s) where
	pure = PureS
	PureS f <*> m = f <$> m
	State k f <*> m = State k $ \s -> f s <*> m

instance Monad (BuildState s) where
	PureS x >>= f = f x
	State k x >>= f = State k $ (f =<<) . x

runState :: BuildState s a -> s -> (a, s)
runState m s = case m of
	PureS x -> (x, s)
	State k x -> x s `runState` k s

getModify :: (s -> s) -> BuildState s s
getModify f = State f PureS

get :: BuildState s s
get = getModify id

modify :: (s -> s) -> BuildState s ()
modify = (>> return ()) . getModify

put :: s -> BuildState s ()
put = modify . const

sampleS :: BuildState Integer Integer
sampleS = do
	modify (+ 5)
	modify (* 3)
	x <- get
	modify $ subtract 99
	return x
