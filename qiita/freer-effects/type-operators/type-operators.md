Freer Effectsが、だいたいわかった: 11-2 TypeOperators拡張
=========================================================

直積と直和
----------

Haskellでは型を組み合わせることができる。
組み合わせかたには「ふたつの型の両方の値をもつ型をつくる」、または、
「ふたつの型の、どちらかの値をもつ型をつくる」というやりかたがある。
それぞれ、「直積」「直和」のようによぶ。

### 直積

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
A、B、Cという、みっつの値をもつT型との直積型は、つぎのようになる。

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

ふたつの型の、どちらかの型の値をもつ型の値は、つぎのように表現できる。

```hs
> Left 'c' :: Either Char Bool
Left 'c'
> :type it
Either Char Bool
```

代数的データ型では、つぎのようになる。

```hs
> data Sum a b = L a | R b deriving Show
> L 'c' :: Sum Char Bool
L 'c'
> :type it
it :: Sum Char Bool
```

これを直和とよぶ。
値の「数」を考えてみよう。
TrueとFalseの、ふたつの値をもつBool型と、
A、B、Cという、みっつの値をもつT型との直和型は、つぎのようになる。

```hs
Either Bool T
```

この型に属する値は、つぎのようになる。

```hs
Left False
Left True
Right A
Right B
Right C
```
このように2個の値をもつ型と、3個の値をもつ型との直和である型は、
5個の値をもつ。「2たす3で5」だ。

直積型と直和型を定義する
------------------------

タプルとEither型は、そのまま直積と直和である。
また、代数的データ型も、そのままで直積と直和である。
なので、ここで新たな「直積型」「直和型」を定義する理由はないが、
説明の都合上、定義することとする。
ファイルproductSum.hsを作成する。

```hs:productSum.hs
data Product a b = Product a b deriving Show
data Sum a b = L a | R b deriving Show

x :: Sum (Product Char Bool) (Product String Integer)
x = L $ Product 'c' True
```

試してみる。

```hs
> :load productSum.hs
> x
L (Product 'c' True)
> :type x
x :: Sum (Product Char Bool) (Product String Integer)
```

値構築演算子
------------

値構築演算子を使うと、直積型はより読みやすくなる。
productSum.hsの、データ型Productの定義を修正する。

```hs:productSum.hs
data Product a b = a :*: b deriving Show
```

値構築演算子は、ふつうの演算子と名前空間がおなじなので、
区別するために、その名前には:(コロン)から、はじまる識別子を使う。
最後にも:(コロン)をつけたのは、そのほうが見た目のバランスがいいからで、
とくに深い意味はない。
サンプルの値xの定義も修正する。

```hs:productSum.hs
x = L $ 'c' :*: True
```

```hs
> :reload
> x
L ('c' :*: True)
> :type x
x :: Sum (Product Char Bool) (Product String Integer)
```

型構築演算子
------------

型構築演算子の結合力
--------------------

まとめ
------
