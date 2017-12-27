Freer Effectsが、だいたいわかった: 11-6 FlexibleInstances拡張
=================================================================

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
	4. [DataKinds拡張](
		../data-kinds/data-kinds.md )
	5. [MultiParamTypeClasses拡張](
		../multi-param-type-classes.md )
	6. FlexibleInstances拡張
	7. OVERLAPSプラグマ
	8. FlexibleContexts拡張
	9. LambdaCase拡張
12. Open Unionを型によって安全にする
13. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
14. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
15. 関数を保管しておくデータ構造による効率化
16. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

Haskell標準での、インスタンス宣言の頭部
---------------------------------------

Haskell標準(98, 2010)でのインスタンス宣言の頭部は、
つぎのような形でなければならない。

```hs
C (T a1 ... an)
```

Cはクラスの名前、Tは型構築子(型シノニムは不可)、nは0以上で、
a1からanまで、すべて異なる型変数である必要がある。

### 許される形

つぎのようなインスタンス宣言は許される。

```hs
instance C T1 where ...
instance C (T2 a b c) where ...
```

### 許されない形

つぎのようなインスタンス宣言は許されない。

```hs
instance C a where ...
instance C (T1 T2) where ...
instance C (T3 (T4 a) b c) where ...
instance C (T5 a b b) where ...
```

インスタンスとされる型は、
型変数である型引数を0個以上とる型構築子でなければならず、
型変数はそれぞれ異なるものでなければならない。

合計をもとめる
--------------

整数のリストであれば、その合計を、
文字列のリストであれば、それらのうち0以上の整数をあらわすものを、
整数として読み出した、その合計を、それぞれもとめるような関数を作りたい。

```hs:total.hs
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Char

class Totalable a where
        total :: a -> Integer

instance Totalable [Integer] where
        total = sum

instance Totalable [String] where
        total [] = 0
        total (s : ss)
                | all isDigit s = read s + total ss
                | otherwise = total ss
```

FlexibleInstances拡張を使わないとき、インスタンス宣言で、
許される形はつぎのようになる。

```hs
instance Totalable [a] where ...
```

ここで、型変数aを具体的な型である、型Integerや型Stringとするために、
FlexibleInstances拡張が必要になる。
試してみよう。

```hs
> :load total.hs
> total [123, 456, 789]
1368
> total ["123", "456", "789"]
1368
```

ちなみに、FlexibleInstances拡張に含まれる拡張として、
TypeSynonymInstances拡張がある。
これは、インスタンス宣言の頭部に型シノニムを使えるようにする。
よって、上の例でも型シノニムStringを使うことが可能になった。

ひっくりかえす
--------------

タプルなら、その第1要素と第2要素とをひっくりかえし、
Either型の値なら、値構築子LeftとRightを入れかえるような関数を作りたい。

```hs:rev.hs
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Revable a where
        rev :: a -> a

instance Revable (a, a) where
        rev (x, y) = (y, x)

instance Revable (Either a a) where
        rev (Left x) = Right x
        rev (Right y) = Left y

tpl :: (Char, Char)
tpl = ('j', 'z')

ethr :: Either Double Double
ethr = Left 1.23
```

試してみる。

```hs
> :load rev.hs
> rev tpl
('z','j')
> rev ethr
Right 1.23
```

このように「ひっくりかえす」ためには、
タプルなら、第1要素と第2要素の型がおなじである必要があり、
Either型の値なら、値構築子LeftとRightととる値の型がおなじである必要がある。

もともとの制限の意味
--------------------

もともとの制限にはどんな意味があったのだろうか。
おそらく、つぎのような意味がある。
もともとの制限をなくしてしまうと、
インスタンス宣言が重複してしまう可能性があるということだ。
たとえば、つぎのような、ふたつのインスタンス宣言は重複してしまう可能性がある。

```hs
instance Foo (Bar Integer) where ...
instance Foo (Bar a) where ...
```

また、つぎのような、ふたつのインスタンス宣言も重複してしまう可能性がある。

```hs
instance Foo (Bar a b b) where ...
instance Foo (Bar a b c) where ...
```

このような重複を避けるために、もともとの制限があったと考えられる。

インスタンス停止規則(Instance termination rule)
-----------------------------------------------

インスタンス宣言に、型制約をつける場合に、満たさなければならない規則がある。
この規則は型制約を追いかけていく作業が、無限ループにならないために存在する。
たとえば、つぎのようなインスタンス宣言は型制約をたどる作業が終了しなくなる。

```hs
instance Foo a => Bar a where ...
instance Bar a => Foo a where ...
```

このようなことを避けるために、つぎのような規則がある。

1. どの型変数も、型制約(constraints on type parameters)への出現回数が
	インスタンス頭部への出力回数を越えない
2. 型構築子と型変数の数の合計(くりかえしも数える)が、
	型制約よりもインスタンス頭部でのほうが大きい
3. 型制約には型関数を使わない

基本的な考えかたとしては、型制約をたどるごとに、
型制約が「より小さく」なっていくということだ。

```hs
instance Foo a => Bar [a] where ...
```

このようになっていれば、(Bar [a])よりも(Foo a)のほうが小さくなる。
すべてにおいて、こうなっていれば、型制約をたどる作業はいつかは終了する。

参考: [GHC 8.2.2 User's Guide 10.8.3.4. Instance termination rules](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-termination )

まとめ
------

Haskell標準では、おそらくインスタンス宣言の重複を避けるために、
インスタンス宣言の頭部の取れる形に制限があった。
その制限を解除するのがFlexibleInstances拡張だ。
ただし、FlexibleInstanced拡張を使ったとしても、
インスタンス停止規則という別の制限は、まだ存在する。
