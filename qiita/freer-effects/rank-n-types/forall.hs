{-# LANGUAGE ExplicitForAll #-}

myMap :: forall a b . (a -> b) -> [a] -> [b]
myMap f xs = map f xs

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = map f xs
