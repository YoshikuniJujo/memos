(>>-) :: Functor f => f a -> (a -> f b) -> f (f b)
(>>-) = flip fmap
