Freer Effectsが、だいたいわかった: 11-4 DataKinds拡張
=====================================================

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
	3. [KindSignatures拡張](
		../kind-signatures/kind-signatures.md )
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

型の型(種類)
------------

型にも型がある。
型の型のことをkind(種類)とよぶ。
値を型づけできる型の種類は\*(スター)だ。
また、型引数をひとつとる型の種類は\* -> \*になる。
\* -> \*種の型は、\*種の型を引数にとり、\*種の型をかえす。
これは、たとえばInteger -> Integer型の値(関数)が、
Integer型の値を引数にとり、Integer型の値をかえすのと、おなじことだ。

たとえば、\* -> \*種の型である型Maybeは、\*種の型である型Charを引数にとり、
\*種の型であるMaybe Charをかえす。

```hs
> :kind Maybe
Maybe :: * -> *
> :kind Char
Char :: *
> :kind Maybe Char
Maybe Char :: *
```

計画
----

DataKinds拡張を使わなくても作れる例をまず挙げて、
そのあとに、DataKinds拡張だと、より安全に書けることを示す。
空リストを型ではじける例が有名だけど、できれば、ほかの例を挙げたい。

「空ではないリスト」以外の例で、いい例を思いつかないな。
何かないかな?
条件は

* カンタン
* ほかの拡張をできるだけ使わない
* 意味がある
	+ ほかのやりかたよりシンプル
	+ あるいは、ほかのやりかたより本質的

かな。

DataKinds拡張を使うというのは、まあだいたいは、
幽霊型について、よりちゃんとした型(種)付けをしたいということだ。
なので、とりあえず幽霊型が役に立つ例を挙げるところから、かもしれない。
