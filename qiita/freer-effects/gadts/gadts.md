Freer Effectsが、だいたいわかった: 5. 一般化代数データ型(GADTs拡張)の解説
=========================================================================

目次
----

0. [導入](../prelude.md)

1. [Freeモナドの概要](../free-monad/free-monad.md)
	* Freeモナドとは
	* FreeモナドでReaderモナド、Writerモナドを構成する
2. [存在型(ExistentialQuantification拡張)の解説](
	../existential-quantification/existentials.md )
3. [型シノニム族(TypeFamilies拡張)の解説](./type-synonym-family.md)
4. [データ族(TypeFamilies拡張)の解説](../type-families/data-family.md)
5. 一般化代数データ型(GADTs拡張)の解説
6. FreeモナドとCoyoneda
	* Coyonedaを使ってみる
	* FreeモナドとCoyonedaを組み合わせる
		+ いろいろなモナドを構成する
7. Freerモナド(Operationalモナド)でいろいろなモナドを構成する
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
8. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
9. 存在型による拡張可能なデータ構造(Open Union)
10. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
11. Open Unionを型によって安全にする
12. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
13. 関数を保管しておくデータ構造による効率化
14. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

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
ちなみに、GADTs拡張には、意味論的には、
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

class SurfaceAreable rh where
        data FaceArea rh
        calcFaceArea :: rh -> FaceArea rh
        getSurfaceArea :: FaceArea rh -> Double
```

FaceArea rhという新しいデータ族を宣言している。
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
                FaceAreaTetra $ fromInteger (a * a) * sin (pi / 3) / 2
	getSurfaceArea (FaceAreaTetra fa) = 4 * fa
```

三角形ABCの面積は、つぎの式でもとめられる。

	(ABの長さ * BCの長さ * sin 角ABC) / 2

よって、正三角形では、1辺の長さをaとして、つぎのようになる。

	(a * a * sin 60度) / 2

#### 正六面体

正六面体では、つぎのようになる。

```hs:regularPolyhedronFamily.hs
data Hexahedron = Hexahedron Integer deriving Show

instance SurfaceAreable Hexahedron where
        data FaceArea Hexahedron = FaceAreaHexa Integer deriving Show
        calcFaceArea (Hexahedron a) = FaceAreaHexa $ a * a
        getSurfaceArea (FaceAreaHexa fa) = 6 * fromInteger fa
```

正六面体の、ひとつの面は正方形なので、一辺の長さを2乗すれば、
その面積になる。
ひとつの面の面積をInteger型の値として保存することで、
Double型の値として保存するよりも「正確な値」とすることができる。
表面積をもとめる段階で、
ほかの形の場合とおなじになるようにDouble型の値としている。

#### 正八面体

正八面体では、つぎのようになる。

```hs:regularPolyhedronFamily.hs
data Octahedron = Octahedron Integer deriving Show

instance SurfaceAreable Octagedron where
        data FaceArea Octahedron = FaceAreaOcta Double deriving Show
        calcFaceArea (Octahedron a) =
                FaceAreaOcta $ fromInteger (a * a) * sin (pi / 3) / 2
        getSurfaceArea (FaceAreaOcta fa) = 8 * fa
```

正四面体での定義と、ほとんどおなじだ。
面の数が4か8かという、ちがいがある。

#### 正十二面体

正十二面体では、つぎのようになる。

```hs:regularPolyhedronFamily.hs
data Dodecahedron = Dodecahedron Integer deriving Show

instance SurfaceAreable Dodecahedron where
        data FaceArea Dodecahedron = FaceAreaDodeca Double deriving Show
        calcFaceArea (Dodecahedron a) =
                FaceAreaDodeca $ fromInteger (a * a) * 5 / (4 * tan (pi / 5))
        getSurfaceArea (FaceAreaDodeca fa) = 12 * fa
```

五角形の面積は、一辺の長さをaとすると、つぎのようになる。

	5 * (a * a) / (4 * tan 36度)

#### 正二十面体

正二十面体では、つぎのようになる。

```hs:regularPolyhedronFamily.hs
data Icosahedron = Icosahedron Integer deriving Show

instance SurfaceAreable Icosahedron where
        data FaceArea Icosahedron = FaceAreaIcosa Double deriving Show
        calcFaceArea (Icosahedron a) =
                FaceAreaIcosa $ fromInteger (a * a) * sin (pi / 3) / 2
        getSurfaceArea (FaceAreaIcosa fa) = 20 * fa
```

これも正四面体と、ほぼおなじ。

#### 試してみよう

対話環境で試してみよう。

```hs
> :load regularPolyhedronFamily.hs
> calcFaceArea $ Hexahedron 3
FaceAreaHexa 9
> getSurfaceArea it
54.0
> calcFaceArea $ Dodecahedron 10
FaceAreaDodeca 172.0477400588967
> getSurfaceArea it
2064.5728807067603
```

### 一般化代数データ型(GADTs)で

さて、データ型FaceArea rhについて考えてみよう。
型変数rhには、モジュール内で定義された正多面体をあらわす型がはいる。
正n面体について、ゆるされるのはn = 4, 6, 8, 12, 20のみである。
つまり、正多面体は、この5種類しかない。
よって、rhにうえで考えた値以外は入らないものとしていい。
つまり、「開いたデータ族」ではなく「閉じたデータ族」として、
表現することが可能だ。
「閉じたデータ族」と同等の表現である一般化代数データ型(GADTs)で、
多面体の表面積をもとめる例を書き直してみよう。

#### それぞれの正多面体をあらわすデータ型

それぞれの正多面体をあらわすデータ型を定義する。

```hs:regularPolyhedronGadts.hs
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RegularPolyhedron where

data Tetrahedron = Tetrahedron Integer deriving Show
data Hexahedron = Hexahedron Integer deriving Show
data Octahedron = Octahedron Integer deriving Show
data Dodecahedron = Dodecahedron Integer deriving Show
data Icosahedron = Icosahedron Integer deriving Show
```

#### ひとつの面の面積をあらわすデータ型

ひとつの面の面積をあらわすデータ型をGADTを使って定義する。

```hs:regularPolyhedronGadts.hs
data FaceArea rh where
        FaceAreaTetra :: Double -> FaceArea Tetrahedron
        FaceAreaHexa :: Integer -> FaceArea Hexahedron
        FaceAreaOcta :: Double -> FaceArea Octahedron
        FaceAreaDodeca :: Double -> FaceArea Dodecahedron
        FaceAreaIcosa :: Double -> FaceArea Icosahedron
```

#### ひとつの面の面積を計算するクラス関数

ひとつの面の面積を計算するクラス関数を定義する。

```hs:regularPolyhedronGadts.hs
class FaceAreable rh where
        calcFaceArea :: rh -> FaceArea rh

instance FaceAreable Tetrahedron where
        calcFaceArea (Tetrahedron a) =
                FaceAreaTetra $ fromInteger (a * a) * sin (pi / 3) / 2

instance FaceAreable Hexahedron where
        calcFaceArea (Hexahedron a) = FaceAreaHexa $ a * a

instance FaceAreable Octahedron where
        calcFaceArea (Octahedron a) =
                FaceAreaOcta $ fromInteger (a * a) * sin (pi / 3) / 2

instance FaceAreable Dodecahedron where
        calcFaceArea (Dodecahedron a) =
                FaceAreaDodeca $ fromInteger (a * a) * 5 / (4 * tan (pi / 5))

instance FaceAreable Icosahedron where
        calcFaceArea (Icosahedron a) =
                FaceAreaIcosa $ fromInteger (a * a) * sin (pi / 3) / 2
```

#### 表面積を取り出す関数

表面積を取り出す関数を定義する。
これは、GADTがデータ族とおおきく異なるところだ。
「型のちがい」をこえて、それぞれ異なるデータ型の値構築子を、
ひとつのデータ型のなかの、異なる値構築子として、
まとめて定義することができる。

```hs:regularPolyhedronGadts.hs
getSurfaceArea :: FaceArea rh -> Double
getSurfaceArea (FaceAreaTetra fa) = 4 * fa
getSurfaceArea (FaceAreaHexa fa) = 6 * fromInteger fa
getSurfaceArea (FaceAreaOcta fa) = 8 * fa
getSurfaceArea (FaceAreaDodeca fa) = 12 * fa
getSurfaceArea (FaceAreaIcosa fa) = 20 * fa
```

#### 試してみる

対話環境で試してみる。

```hs
> :load regularPolyhedronGadts.hs
> getSurfaceArea . calcFaceArea $ Hexahedron 3
54.0
> getSurfaceArea . calcFaceArea $ Dodecahedron 10
2064.5728807067603
```

#### 表面積を整数で

六面体のひとつの面の面積は、整数値として保存されている。
このことの「良さ」を実感するために、
表面積を整数として取り出す関数を書いてみよう。

```hs:regularPolyhedronGadts.hs
getSurfaceAreaI :: FaceArea rh -> Integer
getSurfaceAreaI (FaceAreaTetra fa) = round $ 4 * fa
getSurfaceAreaI (FaceAreaHexa fa) = 6 * fa
getSurfaceAreaI (FaceAreaOcta fa) = round $ 8 * fa
getSurfaceAreaI (FaceAreaDodeca fa) = round $ 12 * fa
getSurfaceAreaI (FaceAreaIcosa fa) = round $ 20 * fa
```

対話環境で試してみる。

```hs
> :reload
> getSurfaceAreaI . calcFaceArea $ Hexahedron 3
54
> getSurfaceAreaI . calcFaceArea $ Dodecahedron 10
2065
```

Double型の値として表面積を計算する場合にも、
Integer型の値として表面積を計算する場合にも、
ムダな型変換をおこなわずにすむように作ることができた。

まとめ
------

一般化代数データ型(GADT)は、
幽霊型を使うテクニックの延長として説明されることがあるが、
ここでは、閉じたデータ族(に存在型を加味したもの)という方向から説明した。
いっしょくたにされてしまうデータ型の結果を、
型によってわけるという方向では説明しなかった。
そうではなく、
別々の型であっても閉じたデータ族であれば、値構築子が追加されることはないので、
その値をあつかう(引数とする)関数は、クラス関数にしなくても、
ふつうの関数として定義できますよという説明とした。

「型族(と存在型)については理解したけれど、GADTって何だろう」
という人向けの説明だ。
より、一般的な説明は下記の「参考」のリンクを参照のこと。

参考
----

[Wikibooks: Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
