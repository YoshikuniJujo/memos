Freer Effectsが、だいたいわかった: 5. 一般化代数データ型(GADTs拡張)の解説
=========================================================================

はじめに
--------

この記事では、一般的な一般化代数データ型(GADT)の解説とは
異なる方向から解説する。
一般的なGADTの解説では、幽霊型を導入として、
幽霊型だと「ここまではできないよね」というところからGADTを導入していく
感じかと思う([Wikibooks: Haskell/GADT](
https://en.wikibooks.org/wiki/Haskell/GADT))。
個人的に、GADTよりもデータ族(data family)のほうが直観的にわかりやすいと感じる。
なので、データ族の説明からはいり、
「GADTはだいたいにおいて、閉じたデータ族という感じ」という解説をした。
ちなみに、GADTs拡張をすると、意味論的には、
ExistentialQuantification拡張も有効にしたのと同等の効果も含まれる。

GADTとは
---------

ここでは、GADTを「閉じたデータ族のようなもの」と考えよう。

独得の記法
----------

GADTには「独得の記法」がある。
標準的なデータ型の定義は、たとえば、つぎのようになる。

```hs
data Foo a b
        = Bar a
        | Baz b
        | Qux Int
```

それぞれの値構築子のあとに、値構築子のとる引数の型を並べる。
おなじことがGADTの記法では、つぎのように表現される。

```hs
data Foo a b where
        Bar :: a -> Foo a b
        Baz :: b -> Foo a b
        Qux :: Int -> Foo a b
```

値構築子の型を指定することで、標準的な記法とおなじことを表現している。

GADTの記法だと、何ができるか
----------------------------

標準的な記法にはできなくて、GADTの記法だとできることがある。
それは、値構築子の結果の型を指定することだ。
たとえば、つぎのようにすることができる。

```hs
data Example a where
        Some :: Int -> Example Int
	Other :: Bool -> Example Bool
```

「開いている」のか「閉じている」のかの、ちがいを考えなければ、
同等のものを、つぎのようなデータ族で書くことができる。

```hs
data family Example a
data instance Example Int = Some Int
data instance Example Bool = Other Bool
```

「閉じている」からできること
----------------------------

「閉じている」ので、値構築子の列挙が可能になる。
これにより、GADTを利用して定義した値を処理する関数を、
型クラスのインスタンス関数ではない、ふつうの関数として定義できる。
うえの例では、つぎのような関数が定義できる。

```hs
fun :: Example a -> a
fun (Some n) = n
fun (Other b) = b
```

これは、つぎのような定義と、だいたい、おなじと考えられる。

```hs
class Fun a where
        fun :: Example a -> a

instance Fun Int where
        fun (Some n) = n

instance Fun Bool where
        fun (Other b) = b
```

XXXの例
-------

### データ族で

### 一般化代数データ型(GADTs)で

参考
----

[Wikibooks: Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
