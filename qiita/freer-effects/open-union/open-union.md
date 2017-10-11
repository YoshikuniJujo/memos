Freer Effectsが、だいたいわかった: 10. 存在型による拡張可能なデータ構造(Open Union)
===================================================================================

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
5. [一般化代数データ型(GADTs拡張)の解説](../gadts/gadts.md)
6. [ランクN多相(RankNTypes拡張)の解説](../rank-n-types/rank-n-types.md)
7. [FreeモナドとCoyoneda](../free-coyoneda/free_coyoneda.md)
	* Coyonedaを使ってみる
	* FreeモナドとCoyonedaを組み合わせる
		+ いろいろなモナドを構成する
8. [Freerモナド(Operationalモナド)でいろいろなモナドを構成する](
	../freer-monad/freer-monad.md )
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
9. [モナドを混ぜ合わせる(閉じた型で)](
	../closed-mix/closed-mix.md )
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
		+ 両方のモナドを一度に処理する
		+ それぞれのモナドを、それぞれに処理する
10. 存在型による拡張可能なデータ構造(Open Union)
11. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
12. Open Unionを型によって安全にする
13. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
14. 関数を保管しておくデータ構造による効率化
15. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

はじめに
--------

存在型を使用した開かれた直和型について説明する。
[存在型(ExistentialQuantification拡張)の解説](
	../existential-quantification/existentials.md )
も参照のこと。

いろいろな型の値を含むリスト
----------------------------

たとえば、Haskellでは、つぎのようなリストを定義することはできない。

```hs
[123, True, (), 'c']
```

こういうことをしたければ、つぎのようなデータ型を作る必要がある。

```hs
data Value
        = Unit ()
        | Bool Bool
        | Integer Integer
        | Char Char
```

このように定義しておけば、つぎのようなリストを定義することができる。

```hs
[Integer 123, Bool True, Unit (), Char 'c']
```

リストに含む型がはじめからわかっていれば、これでいい。
それがあらかじめわかっていないとき、開かれた直和型が必要になる。

```hs:openUnion.hs
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data UnionValue = forall x . UnionValue x

hetero :: [UnionValue]
hetero = [
        UnionValue (123 :: Integer), UnionValue True,
        UnionValue (), UnionValue 'c' ]

fromHetero :: [UnionValue] -> (Integer, Bool, (), Char)
fromHetero [UnionValue n, UnionValue b, UnionValue u, UnionValue c] = (
	unsafeCoerce n, unsafeCoerce b, unsafeCoerce u, unsafeCoerce c )
```

このデータ型であれば、いくらでも「値構築子を追加する」のとおなじことができる。

```hs:openUnion.hs
doubleValue :: UnionValue
doubleValue = UnionValue (123 :: Double)
```

これは、つぎのようにデータ型Valueに値構築子を追加したようなものである。

```hs
data Value
        = Unit ()
        | Bool Bool
        | Integer Integer
        | Char Char
        | Double Double
```

追加できる文脈
--------------
