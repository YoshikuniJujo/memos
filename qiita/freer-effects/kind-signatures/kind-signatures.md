Freer Effectsが、だいたいわかった: 11-3 KindSignatures拡張
==========================================================

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
10. [存在型による拡張可能なデータ構造(Open Union)](
	../open-union/open-union.md )
11. 追加の言語拡張
	1. [ScopedTypeVariables拡張](
		../scoped-type-variables/scoped-type-variables.md )
	2. [TypeOperators拡張](
		../type-operators/type-operators.md )
	3. KindSignatures拡張
	4. DataKinds拡張
	5. MultiParamTypeClasses拡張
	6. FlexibleInstances拡張
	7. OVERLAPSプラグマ
12. Open Unionを型によって安全にする
13. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
14. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
15. 関数を保管しておくデータ構造による効率化
16. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

値に型付けできる型と、できない型
--------------------------------

たとえば、型Boolや型Charは値を型付けできる。
しかし、型Maybeは、そのままでは値を型付けできない。
Maybe Charのように、型引数をひとつとる必要がある。
型Eitherであれば、Either Bool Charのように、型引数をふたつとる必要がある。
対話環境でみてみよう。

```hs
> False :: Bool
False
> 'c' :: Char
'c'
> Just 'c' :: Maybe Char
Just 'c'
> Left True :: Either Bool Char
Left True
```

型の型(種類)
------------

型にも型がある。
それを種類(kind)とよぶ。
対話環境で型の種類をみてみよう。

```hs
> :type False
False :: Bool
> :kind Bool
Bool :: *
```

値Falseの型はBoolで、型Boolの種類は\*だ。
ここで、型Maybeの種類をみてみよう。

```hs
> :kind Maybe
Maybe :: * -> *
> :kind Bool
Bool :: *
> :kind Maybe Bool
Maybe Bool :: *
```

たとえば、(Int -> Int)型の関数にInt型の値を引数としてあたえると、
Int型の値に評価される。
それとおなじように、(\* -> \*)種の型構築子に\*種の型をあたえると、
\*種の型になる。
型引数をふたつとる型Eitherについてもみてみよう。

```hs
> :kind Either
Either :: * -> * -> *
> :kind Either Bool
Either Bool :: * -> *
> :kind Either Bool Char
Either Bool Char :: *
```

種推論
------

\*種の型でないと値を型づけすることはできない。
なので、たいていにおいて、型の種は機械的に推論できる。
たとえば、つぎのような型宣言があったとする。

```hs
f :: a -> m a
```

すると、型aは\*種の型であり、型mは(\* -> \*)種の型であることがわかる。

(続く)
