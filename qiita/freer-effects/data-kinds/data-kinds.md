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

はじめに
--------

DataKinds拡張について学ぶ。

値には型がある。
型には種類がある。
DataKinds拡張を有効にしておくと、
「値 - 型」を定義したときに、それに対応する「型 - 種類」が定義される。

と、いうわけだが、ここまで読んで「なるほど、なるほど」となったかたは、
続きを読む必要はない。
このあとは、具体的な例で説明していく。

### 言葉について

kindをここでは「種類」と訳した。
一般的には「種」と呼ぶ。
「種」よりも「種類」のほうが、日常的な言葉なので、
とっつきやすいという配慮だ。

タプルでリスト
--------------

Lisperにとって、リストは連鎖したコンスだ。
コンスというものは、まあだいたい2要素タプルのようなものだ。
なので、LisperにとってのリストをHaskellで書くと、つぎのようになる。

```hs:tupleList0.hs
ex1 :: (Integer, (Double, (Bool, ())))
ex1 = (123, (3.14, (True, ())))
```

ついでにcarとcdrを定義しておこう。

```hs:tupleList0.hs
car :: (a, b) -> a
car = fst

cdr :: (a, b) -> b
cdr = snd
```

試してみよう。

```hs
> :load tupleList0.hs
> ex1
(123,(3.14,(True,())))
> car ex1
123
> car . cdr $ ex1
3.14
> car . cdr . cdr $ ex1
True
> cdr . cdr . cdr $ ex1
()
```

「タプルでリスト」の型をまとめる
--------------------------------

「タプルでリスト」の型に別名をつけて、ひとつにまとめたい。
型の別名には「型のリスト」が必要になる。
まずは「型のリスト」を作ってみよう。

```hs:tupleList.hs
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data Nil
data a :^ b
infixr 5 :^
```

型Nilや型a :^ bは、値を持たない型だ。
このような型はHaskell 98標準では許されず、
Haskell 2010(または、GHCのEmptyDataDecls拡張)で使えるようになった。
これらは、値の型付けには使われず、
型構築子の引数として使われる。
試してみる。

```hs
> :load tupleList.hs
> :set -XTypeOperators
> :kind Integer :^ Double :^ Bool :^ Nil
Integer :^ Double :^ Bool :^ Nil :: *
```

これで、型のリストができた。
これを引数とする型シノニムTupleListを定義する。

```hs:tupleList.hs
type family TupleList a where
        TupleList Nil = ()
        TupleList (a :^ b) = (a, TupleList b)
```

試してみる。

```hs
> :reload
> :kind TupleList
TupleList :: * -> *
> :kind! TupleList (Integer :^ Double :^ Bool :^ Nil)
TupleList (Integer :^ Double :^ Bool :^ Nil) :: *
= (Integer, (Double, (Bool, ())))
```

ちゃんと期待している型に展開(評価)される。
値ex1の型付けに、この型シノニムを使ってみよう。

```hs:tupleList.hs
ex1 :: TupleList (Integer :^ Double :^ Bool :^ Nil)
ex1 = (123, (3.14, (True, ())))
```

型の種付けが、ゆるゆるだ
------------------------

型構築子TupleListや型演算子(:^)といった型の種を、
もういちど、みてみよう。

```hs
> :kind TupleList
TupleList :: * -> *
> :kind (:^)
(:^) :: * -> * -> *
```

TupleListは\*(スター)種の型をとって、\*種の型をかえす。
(:^)は\*種の型を、ふたつとって、\*種の型をかえす。
つまり、つぎのような型も種エラーにならない。

```hs
> :kind TupleList Integer
TupleList Integer :: *
> :kind Integer :^ Double
Integer :^ Double :: *
```

型構築子TupleListの引数は「型のリスト」であってほしいし、
型演算子(:^)の第2引数もまた、「型のリスト」であってほしい。
しかし、それ以外の種類の型を引数としても、種エラーはおこらない。
これは、種チェックが期待しているほどに厳密に働いていないことになる。

型に、ちゃんとした種類付けをする
--------------------------------

\*種ではなく、型のリストを意味する型に、ちゃんとした種類を用意する。
ここでDataKinds拡張を利用する。

```hs:tupleListDK.hs
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data List a = Nil | a :^ (List a)
infixr 5 :^
```

List a型の値として、値Nilと、引数をふたつとる値構築子(:^)とを定義した。
ここで、DataKinds拡張の力が効果が発動する。
すると、「型 - 値」の世界での定義は、
「種類 - 型」の世界にも双子の定義を出現させる。
つまり、List a種の型として、型Nilと、引数をふたつとる型構求子(:^)とが定義される。

それぞれを確かめてみよう。

```hs
> :load tupleListDK.hs
> :type Nil
Nil :: List a
> :kind Nil
Nil :: List a
```

「型 - 値」の世界に「Nil - List a」があり、
「種類 - 型」の世界にも「Nil - List a」があることがわかる。
このような、「型 - 値」に対応して作られた「種類 - 型」を、
「昇格された種類 - 昇格された型」のように呼ぶ。
型シノニムTupleListや、値ex1の定義はtupleList.hsのものと、だいたい、おなじだ。

```hs:tupleListDK.hs
type family TupleList a where
        TupleList 'Nil = ()
        TupleList (a ':^ b) = (a, TupleList b)

ex1 :: TupleList (Integer ':^ Double ':^ Bool ':^ 'Nil)
ex1 = (123, (3.14, (True, ())))
```

tupleList.hsとのちがいは、Nilや(:^)に'(クォート)が付いているところだ。
これは、この場合にはつけなくても問題ないが、
必須のときもあるので、つけておいたほうが混乱しない。
「昇格された型」であることの印だ。

(自分への覚え書き: あとで確認すること!
「昇格された型」という言いかたは「昇格された結果としてできた型」の意味
で使って問題ないか)

```hs
> :kind Nil
Nil :: List a
> :kind (:^)
(:^) :: a -> List a -> List a
> :kind TupleList
TupleList :: List * -> *
> :kind TupleList Integer
(種エラーが生じる)
> :kind Integer :^ Bool
(種エラーが生じる)
```

種チェックが適切に行われていることがわかる。

デフォルトのリスト([])も昇格している
------------------------------------

デフォルトのリスト([])も昇格しているので、
わざわざList種を定義する必要はない。
また、リストに対する構文糖も「型 - 値」の場合とおなじように、
「種類 - 型」の世界でも使うことができる。

```hs:tupleListDKL.hs
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

type family TupleList a where
        TupleList '[] = ()
        TupleList (a ': b) = (a, TList b)

ex1 :: TupleList '[Integer, Double, Bool]
ex1 = (123, (3.14, (True, ())))
```

まとめ
------

型構築子の引数として使うために、値を持たない型を定義することができる。
その場合、その型は\*種の型として定義される。
ときには、より限定的な種類の型として定義したいこともある。
そのようなときに使えるのが、DataKinds拡張だ。
「型 - 値」として定義したものとおなじものを、
「種類 - 型」の世界でも使えるようになる。
このような操作を「昇格」と呼ぶ。
Haskellのリストやタプルも昇格されていて、
「型 - 値」の世界とおなじ構文糖を使うこともできる。
昇格された型には'(クォート)で印をつけておくと、わかりやすい
(リストの場合など必須のこともある)。

<!--

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

-->
