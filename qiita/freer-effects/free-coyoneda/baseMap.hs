{-# LANGUAGE RankNTypes #-}

import Coyoneda

coyonedaBaseMap :: (forall x . t x -> t x) -> Coyoneda t a -> Coyoneda t a
coyonedaBaseMap f (Coyoneda tx g) = Coyoneda (f tx) g

coyonedaFromBase :: (forall x . t x -> b) -> Coyoneda t a -> b
coyonedaFromBase f (Coyoneda tx g) = f tx
