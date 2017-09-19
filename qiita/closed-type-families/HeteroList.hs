{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList (empty, (.:), get) where

data a :.: b = a :.: b deriving Show

class Get v vs where
	get :: vs -> v

instance Get v (v :.: vs) where
	get (x :.: _) = x
instance {-# OVERLAPPABLE #-} Get v vs => Get v (_w :.: vs) where
	get (_ :.: xs) = get xs

type family Elem t ts where
	Elem _ () = 'False
	Elem t (t :.: _) = 'True
	Elem t (_ :.: ts) = Elem t ts

infixr 5 .:

empty :: ()
empty = ()

(.:) :: Elem t ts ~ 'False => t -> ts -> (t :.: ts)
(.:) = (:.:)
