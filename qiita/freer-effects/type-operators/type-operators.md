Freer Effectsが、だいたいわかった: 11-2 TypeOperators拡張
=========================================================

直積と直和
----------

### 直積

Haskellでは値を組み合わせることができる。
ふたつの型の両方の値をもつ値は、つぎのように表現できる。

```hs
> ('c', True)
('c',True)
> :type it
it :: (Char, Bool)
```

代数的データ型では、つぎのようになる。

```hs
> data Product a b = Product a b deriving Show
> Product 'c' True
Product 'c' True
> :type it
it :: Product Char Bool
```

これを直積とよぶ。
値の「数」を考えると「積」であることが直観的によくわかる。
TrueとFalseとの、ふたつの値をもつBool型と、
A, B, Cという、みっつの値をもつT型との直積型は、つぎのようになる。

```hs
(Bool, T)
```

この型に属する値は、つぎのようになる。

```hs
(False, A)
(False, B)
(False, C)
(True, A)
(True, B)
(True, C)
```

このように2個の値をもつ型と、3個の値をもつ型との直積である型は、
6個の値をもつ。「2かける3で6」だ。

### 直和

値構築演算子
------------

型構築演算子
------------

型構築演算子の結合力
--------------------

まとめ
------
