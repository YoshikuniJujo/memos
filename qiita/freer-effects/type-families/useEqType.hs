{-# LANGUAGE TypeFamilies, DataKinds #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Equal x y where
	Equal a a = 'True
	Equal a b = 'False
