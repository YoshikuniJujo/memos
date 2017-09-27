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

正多面体の表面積の例
--------------------

### 何を作るか

正多面体の表面積を計算するコードを書く。
正多面体の、ひとつの面の面積を、まずはもとめて、
それに面の数をかけることで表面積をもとめる。
正多面体は辺の長さで表現する。
辺の長さは整数のみとする。
正六面体では、結果がかならず整数となり、
正確な値を計算することができる。
そこで、正六面体については、ひとつの面の面積を整数で表現するようにする。

### データ族で

まずは、データ族を使って、書いてみよう。
「表面積をもとめられる」という性質をあらわす型クラスを作成する。

```hs:regularPolyhedronFamily.hs
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RegularPolyhedron where

class SurfaceAreable sa where
        data FaceArea sa
        calcFaceArea :: sa -> FaceArea sa
        getSurfaceArea :: FaceArea sa -> Double
```

FaceArea saという新しいデータ族を宣言している。
これは、ひとつの面の面積をあらわす型だ。
クラス関数calcFaceAreaは正多面体をあらわす値から、
ひとつの面の面積をあらわす値を計算する。
クラス関数getSurfaceAreaは、ひとつの面の面積から、
表面積をDouble型の値として取り出す。

#### 正四面体

正四面体をあらわす型を定義して、
それを型クラスSurfaceAreableのインスタンスにする。

```hs:regularPolyhedronFamily.hs
data Tetrahedron = Tetrahedron Integer deriving Show

instance SurfaceAreable Tetrahedron where
        data FaceArea Tetrahedron = FaceAreaTetra Double deriving Show
        calcFaceArea (Tetrahedron a) =
                FaceAreaTetra $ fromIntegral (a * a) * sin (pi / 3) / 2
	getSurfaceArea (FaceAreaTetra fa) = 4 * fa
```

三角形ABCの面積は、つぎの式でもとめられる。

	(ABの長さ * BCの長さ * sin 角ABC) / 2

よって、正三角形では、1辺の長さをaとして、つぎのようになる。

	(a * a * sin 60度) / 2

#### 正六面体

#### 正八面体

#### 正十二面体

#### 正二十面体

### 一般化代数データ型(GADTs)で

参考
----

[Wikibooks: Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
