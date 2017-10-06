{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

data SE s e a where
	Get :: SE s e s
	Put :: s -> SE s e ()
	Exc :: e -> SE s e a

get :: Freer (SE s e) s
get = freer Get

put :: s -> Freer (SE s e) ()
put = freer . Put

modify :: (s -> s) -> Freer (SE s e) ()
modify f = put . f =<< get

throwError :: e -> Freer (SE s e) a
throwError = freer . Exc

runSE :: Freer (SE s e) a -> s -> Either e (a, s)
runSE m s = case m of
	Pure x -> Right (x, s)
	Get `Bind` k -> runSE (k s) s
	Put s' `Bind` k -> runSE (k ()) s'
	Exc e `Bind` _k -> Left e

catchError :: Freer (SE s e) a -> (e -> Freer (SE s e) a) -> Freer (SE s e) a
m `catchError` h = case m of
	Exc e `Bind` _k -> h e
	_ -> m

safeDiv :: Integer -> Integer -> Freer (SE s String) Integer
safeDiv n 0 = throwError $ show n ++ " is divided by 0"
safeDiv n m = return $ n `div` m

sample :: Freer (SE Integer String) Integer
sample = do
	a <- get
	modify (subtract 5)
	modify (* 2)
	b <- get
	c <- 60 `safeDiv` b `catchError` const (return 50000)
	put a
	modify (subtract 3)
	d <- get
	e <- 250 `safeDiv` d
	return $ c + e

runState :: Freer (SE s e) a -> s -> Freer (SE s e) (a, s)
runState m s = case m of
	Pure x -> Pure (x, s)
	Get `Bind` k -> runState (k s) s
	Put s' `Bind` k -> runState (k ()) s'
	mx `Bind` k -> mx `Bind` ((`runState` s) . k)

runError :: Freer (SE s e) a -> Freer (SE s e) (Either e a)
runError = \case
	Pure x -> Pure $ Right x
	Exc e `Bind` _k -> return $ Left e
	mx `Bind` k -> mx `Bind` (runError . k)

runId :: Freer (SE s e) a -> a
runId = \case
	Pure x -> x
	_ -> error "remain State or Error"
