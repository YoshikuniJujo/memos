Freer Effectsが、だいたいわかった: 4. データ族(TypeFamilies拡張)の解説
======================================================================

目次
----

0. [導入](../prelude.md)

1. [Freeモナドの概要](../free-monad/free-monad.md)
	* Freeモナドとは
	* FreeモナドでReaderモナド、Writerモナドを構成する
2. [存在型(ExistentialQuantification拡張)の解説](
	../existential-quantification/existentials.md )
3. [型シノニム族(TypeFamilies拡張)の解説](./type-synonym-family.md)
4. データ族(TypeFamilies拡張)の解説
5. [一般化代数データ型(GADTs拡張)の解説](../gadts/gadts.md)
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

データ族
--------

### データ族とは

型シノニムに対して、型シノニム族があるように、
代数的データ型の定義に対しては、データ族がある。
データ族は型シノニム族とおなじように、TypeFamilies拡張で使えるようになる。

型シノニム族とデータ族のちがいは、
型シノニムの定義と、代数的データ型の定義とのちがいとおなじだ。
それぞれのインスタンスが、
型シノニム族では既存の型に対する別名となるのに対して、
データ族では新しい型になるということだ。

それぞれの型に対して、それぞれに専用のデータ型を用意したいようなときに、
使うことができる。

### データ族の例

データ族の定義の書きかたは、型シノニム族の定義と、あまり変わらない。
データ族を定義してみよう。
ファイルdataFamily.hsを、つぎのように作成する。

```hs:dataFamily.hs
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data family Foo x

data instance Foo Int = FooInt Bool Char
data instance Foo Char = FooChar Double Integer
```

値構築子の型をみてみよう。

```hs
> :load dataFamily.hs
> :type FooInt
FooInt :: Bool -> Char -> Foo Int
> :type FooChar
FooChar :: Double -> Integer -> Foo Char
```

このような定義は、つぎのような定義とよく似ている。
ファイルnoDataFamily.hsを作成する。

```hs:noDataFamily.hs
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Foo
        = FooInt Bool Char
        | FooChar Double Integer
```

値構築子の型をみてみよう。

```hs
> :load noDataFamily.hs
> :type FooInt
FooInt :: Bool -> Char -> Foo
> :type FooChar
FooChar :: Double -> Integer -> Foo
```

これらは、よく似ているが、データ族では値構築子の追加に対して、開かれている。
つまりFoo BoolやFoo Integerなどが追加される可能性がある。
よって、つぎのように、クラスに属さない関数を書くことはできない。

```hs
useFoo :: Foo a -> ...
useFoo (FooInt x y) = ...
useFoo (FooChar x y) = ...
```

データ属Fooを利用する関数は、クラス関数として書く必要がある。
ファイルdataFamily.hsに追加する。

```hs:dataFamily.hs
class UseFoo a where
        useFoo :: a -> Foo a -> String

instance UseFoo Int where
        useFoo x (FooInt y z) = show x ++ " " ++ show y ++ " " ++ show z

instance UseFoo Char where
        useFoo x (FooChar y z) = show x ++ " " ++ show y ++ " " ++ show z
```

試してみよう。

```hs
> :load dataFamily.hs
> useFoo 123 (FooInt True 'c')
"123 True 'c'"
> useFoo 'c' (FooChar pi 12345)
"'c' 3.141592653598979 12345"
```

### 型クラスに関連づけられたデータ族

データ族は、たいてい、クラス関数によって処理される。
なので、データ族を型クラスに関連づける書きかたが用意されている。
うえの例では、つぎのようになる。

```hs
class UseFoo a where
        data Foo a
        useFoo :: a -> Foo a -> String

instance UseFoo Int where
	data Foo Int = FooInt Bool Char
	useFoo x (FooInt y z) = show x ++ " " ++ show y ++ " " ++ show z
```

だいたいにおいて、こちらの書きかたのほうが、コードがわかりやすくなる。
ここでは「書きかた」をみるために意味のない例を挙げた。
データ族の意味のある使用例は「[決定性有限オートマトンを実装する](
	../../data-families/automaton.md)」を参照のこと。

### 閉じたデータ族?

さて、データ族を処理するには型クラスが必要になる。
それは、データ族が値構築子の追加に対して開いている、つまり、
値構築子の追加がどこでも、モジュールをまたいででも、できるからだ。
値構築子を列挙することができないので、通常の関数ではあつかえない。

それならば閉じたデータ族ならばどうか。
値構築子が列挙できるので、通常の関数であつかえるはずだ。
たとえば、つぎのような定義ができるはずだ。

```hs
data family Foo where
        Foo Int = FooInt Bool Char
	Foo Char = FooChar Double Integer

useFoo :: a -> Foo a -> String
useFoo x (FooInt y z) = ...
useFoo x (FooChar y z) = ...
```

しかし、このような書きかたは許されていない。
この、「閉じたデータ族」と同等な定義ができるようにするのが、
GADTs拡張である。

参考
----

[Wikibooks: GHC/Type families](https://wiki.haskell.org/GHC/Type_families)
