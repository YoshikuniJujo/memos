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

とりあえず僕の知るかぎりでは2つの側面がある。

* 型変数のスコープを広げる
* パターンマッチの変数に型シグネチャをつけることができる

とりあえず後者の説明はしない。
前者の説明だけする。
