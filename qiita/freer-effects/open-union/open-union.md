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
11. Open Unionを型によって安全にする
12. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
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
{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Unsafe.Coerce

data UnionValue = forall x . UnionValue x

hetero :: [UnionValue]
hetero = [
        UnionValue (123 :: Integer), UnionValue True,
        UnionValue (), UnionValue 'c' ]

fromHetero :: [UnionValue] -> (Integer, Bool, (), Char)
fromHetero [UnionValue n, UnionValue b, UnionValue u, UnionValue c] = (
	unsafeCoerce n, unsafeCoerce b, unsafeCoerce u, unsafeCoerce c )
```

対話環境で試してみよう。

```hs
> :load openUnion.hs
> fromHetero hetero
(123,True,(),'c')
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

ここで、[モナドを混ぜ合わせる(閉じた型で)](
	../closed-mix/closed-mix.md )
でみた、データ型SEについて考える。

```hs
data SE s e a where
        Get :: SE s e s
        Put :: s -> SE s e ()
        Exc :: e -> SE s e a
```

これに、Wrierモナドの機能を追加して、つぎのようなデータ型とした。

```hs
data SE s e w a where
        Get :: SE s e w s
        Put :: s -> SE s e w ()
        Exc :: e -> SE s e w a
        Writer :: w -> SE s e w ()
```

おなじことを開いた直和型を使って実装してみよう。
ここで、「開かれてい」てほしいのは機能または文脈であり、
型変数aであらわされる型の値である、かえされる値については、
開かれている必要はないことに注意する。

```hs:openUnion.hs
data Union a = forall t . Union (t a)
```

まずは、状態モナドの機能と、エラーモナドの機能とを、
この型でまとめてみよう。

```hs:openUnion.hs
data State s a where
        Get :: State s s
        Put :: s -> State s ()

data Exc e a where
        Exc :: e -> Exc e a
	deriving Show

effects :: [Union ()]
effects = [Union $ Put (123 :: Integer), Union $ Exc "hello"]
```

データ型Excでだけ、deriving Showとしているのは、
「試してみる都合上」だ。
さらにWriterモナドの機能を追加してみる。
データ型Writerを定義して、サンプルのリストeffectsを編集する。

```hs:openUnion.hs
data Writer w a where
        Writer :: w -> Writer w ()

effects :: [Union ()]
effects = [
	Union $ Put (123 :: Integer),
	Union $ Exc "hello",
	Union $ Writer "world" ]

fromUnion :: Union a -> t a
fromUnion (Union tx) = tx
```

Union型の値から、なかみを取り出す関数fromUnionも定義した。
型エラーなどなく読み込めることを確認する。

```hs
> :load openUnion.hs
> :type effects
effects :: [Union ()]
> fromUnion $ effects !! 1 :: Exc String ()
Exc "hello"
```

データ型State、Exc、Writerのみっつを、
おなじデータ構造に格納することができ、
かつ、型がわかっていれば、そこから取り出せるということがわかった。

まとめ
------

存在型を使って、開かれた直和型を作ることができる。
まずは、ふつうの開かれた型を作り、
それから、「文脈についてだけ」開かれた直和型を作ってみた。
