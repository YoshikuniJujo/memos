memo
====

ScopedTypeVariables拡張が必要。
これは以下のコードで必要になる。

```hs
newtype P (t :: * -> *) (ts :: [* -> *]) = P { unP :: Word }

class Member (t :: * -> *) (ts :: [* -> *]) wehre elemNo :: P t ts
instance Member (t ': ts) where
	elemNo = P 0
instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)
```
