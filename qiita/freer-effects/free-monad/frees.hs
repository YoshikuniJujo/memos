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
