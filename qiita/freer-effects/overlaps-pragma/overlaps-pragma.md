Freer Effectsが、だいたいわかった: 11-7 OVERLAPSプラグマ
========================================================

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
		../multi-param-type-classes/multi-param-type-classes.md )
	6. [FlexibleInstances拡張](
		../flexible-instances/flexible-instances.md )
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

重複するインスタンス宣言
------------------------

### 一方が他方の、真部分集合になる例

ふたつのインスタンス宣言について、
一方が、もう他方を含むような場合について考える
(ただし、両者がおなじ範囲ではない)。
たとえば、つぎのような例だ。

```hs:rejectOverlaps.hs
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
        f :: a -> String

instance Foo [a] where
        f _ = "instance Foo [a] where"

instance Foo [Integer] where
        f _ = "instance Foo [Integer] where"
```

試してみよう。

```hs
> :load rejectOverlaps.hs
> f ['a', 'b', 'c']
"instance Foo [a] where"
> f [3 :: Integer, 4, 5]

<interactive>:3:1: error:
    ・ Overlapping instances for Foo [Integer] arising from a use of 'f'
      Mathcing instances:
        instance [safe] Foo [Integer]
          -- Defined at ...
        instance [safe] Foo [a]
          -- Defined at ...
    ・ In the expression: f [3 :: Integer, 4, 5]
       In an equation for `it': it = f [3, 4, 5]
```

適用する関数を選ぶときに、候補になるインスタンス宣言が複数あるため、
エラーになる。

### 交わるが、たがいに部分集合にならない例

どちらも、たがいの部分集合にならない例。

```hs:rejectIncoherent.hs
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a b where
        f :: a -> b -> String

instance Foo Integer b where
        f _ _ = "instance Foo Integer b where"

instance Foo a Char where
        f _ _ = "instance Foo a Char where"
```

対話環境で試してみよう。

```hs
> :load rejectIncoherent.hs
> f (123 :: Integer) False
"instance Foo Integer b where"
> f "hello" 'c'
"instance Foo a Char where"
> f (123 :: Integer) 'c'

<interactive>:4:1: error:
    ・ Overlapping instances for Foo Integer Char
         arising from a use of `f'
       Matching instances:
         instance [safe] Foo a Char
           -- Defined at ...
         instance [safe] Foo Integer b
           -- Defined at ...
    ・ In the expression: f (123 :: Integer) 'c'
       In an equation for `it': it = f (123 :: Integer) 'c'
```

こちらも、候補になるインスタンス宣言が複数あるため、エラーとなる。

言語拡張
--------

インスタンス宣言において、重なり合う部分を使用することはできない。
これを可能にするのが、OverlappingInstances拡張やIncoherentInstances拡張だ。
これらの言語拡張は現在では非推奨になっている。
言語拡張ではなく、後で紹介するプラグマを使うことで、
インスタンス宣言の重複について、より細かく指定できる。

### OverlappingInstances拡張

#### ひとつめの例

上記のひとつめの例はOverlappingInstances拡張によって、
エラーではなくなる。

```hs:overlapingInstances.hs
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
        f :: a -> String

instance Foo [a] where
        f _ = "instance Foo [a] where"

instance Foo [Integer] where
        f _ = "instance Foo [Integer] where"
```

試してみよう。

```hs
> :load overlappingInstances.hs
> f ['a', 'b', 'c']
"instance Foo [a] where"
> f [3 :: Integer, 4, 5]
"instance Foo [Integer] where"
```

より特定的な(つまり、部分集合となっているほうの)インスタンス宣言が使われる。

#### ふたつめの例

ふたつめの例についても、みてみよう。

```hs:overlappingInstances2.hs
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a b where
        f :: a -> b -> String

instance Foo Integer b where
        f _ _ = "instance Foo Integer b where"

instance Foo a Char where
        f _ _ = "instance Foo a Char where"
```

試してみる。

```hs
> :load overlappingInstances2.hs
> f (123 :: Integer) False
"instance Foo Integer b where"
> f "hello" 'c'
"instance Foo a Char where"
> f (123 :: Integer) 'c'

<interactive>:4:1: error:
    ・ Overlapping instances for Foo Integer Char
         arising from a use of `f'
       Matching instances:
         instance [overlap ok] [safe] Foo a Char
           -- Defined at overlappingInstances2.hs:10:10
         instance [overlap ok] [safe] Foo Integer b
           -- Defined at overlappingInstances2.hs:7:10
    ・ In the expression: f (123 :: Integer) 'c'
       In an equation for `it': it = f (123 :: Integer) 'c'
```

OverlappingInstances拡張では、
一方が他方の真の部分集合だったときだけ、
より特定的なほうのインスタンス宣言が、
それを含むインスタンス宣言を上書きするように解釈される。
この例のように、たがいに相手の部分集合にならないような場合には、
重なる部分については、エラーになる。

### IncoherentInstances拡張

#### ひとつめの例

言語拡張のOverlappingInstancesのところをIncoherentInstances拡張に変える。

```hs:incoherentInstances.hs
{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class Foo a where
        f :: a -> String

instance Foo [a] where
        f _ = "instance Foo [a] where"

instance Foo [Integer] where
        f _ = "instance Foo [Integer] where"
```

#### ふたつめの例

### 言語拡張での問題点

プラグマ
--------

### OVERLAPPABLEプラグマ

### OVERLAPPINGプラグマ

### OVERLAPSプラグマ

### INCOHERENTプラグマ
