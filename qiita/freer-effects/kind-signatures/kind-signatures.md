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

種推論がうまくいかない例
------------------------

たとえば、自分の選んだ型だけに適用したい関数というものを考える。
ここではリストとIOのみに使える関数funを考えよう。

```hs:MyMonad.hs
module MyMonad (fun) where

class MyMonad m

fun :: (Monad m, MyMonad m) => m a -> m b -> m b
fun = (>>)

instance MyMonad []
instance MyMonad IO
```

クラスMyMonadを公開していないので、
このクラスには、インスタンスを追加することができない。
よって、関数funはリストとIOにしか適用できない、はずだ。
対話環境に読み込んでみよう。

```hs
> :load MyMonad.hs
([]やIOに引数が足りないというエラーメッセージ)
```

class MyMonad mで、型変数mは値の型づけに使われていない。
そのため、種推論がはたらかない。
とりあえず、推論できない型は\*種の型とされるようだ。
うまく動くようにするには、まずファイルの先頭に言語拡張プラグマを追加する。

```hs:MyMonad.hs
{-# LANGUAGE KindSignatures #-}
```

そのうえでクラスMyMonadの定義を、つぎのように変更する。

```hs:MyMonad.hs
class MyMonad (m :: * -> *)
```

対話環境で試してみよう。

```hs
> :reload
> fun [1, 2, 3] [4, 5, 6]
[4,5,6,4,5,6,4,5,6]
> fun (print 8) (putStrLn "hello")
8
hello
> fun (Just 8) (Just 9)
(エラーになる)
```

まとめ
------

値には型があるように、型には種類がある。
たいていにおいて、型の種類は機械的に推論できる。
しかし、種推論がうまく働かないこともある。
そのようなときには、KindSignatures拡張によって、
型の種を明示的に注釈することができる。
ここでは、人工的な例を挙げたが、
ほかの言語拡張を利用して、いろいろと複雑なことをやりだすと、
種注釈が必要な場面に、しばしば、つきあたることになる。
