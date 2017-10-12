module OpenUnion where

data Union a = forall t . Union (t a)
