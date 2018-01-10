{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a

instance {-# OVERLAPPABLE #-} Foo () where

instance {-# OVERLAPPING #-} Foo Bool where

instance {-# OVERLAPS #-} Foo Char where

instance {-# INCOHERENT #-} Foo Integer where
