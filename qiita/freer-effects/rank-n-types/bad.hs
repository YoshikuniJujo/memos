{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- fun :: ([a] -> [a]) -> [Integer] -> [Bool] -> ([Integer], [Bool])
-- fun f ns bs = (f ns, f bs)

rev :: [Integer] -> [Char] -> ([Integer], [Char])
rev ns bs = (r ns, r bs)
	where
	r :: [a] -> [a]
	r = reverse
