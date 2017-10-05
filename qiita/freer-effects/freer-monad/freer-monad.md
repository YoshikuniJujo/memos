Freer Effectsが、だいたいわかった: 8. Freerモナド(Operationalモナド)で、いろいろなモナドを構成する
==================================================================================================

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
8. Freerモナド(Operationalモナド)でいろいろなモナドを構成する
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
9. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
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

FreeモナドとCoyonedaをくっつける
--------------------------------

FreeモナドとCoyonedaをくっつけてみる。
まずはCoyonedaを、データ型ではなく型シノニムにする。

```hs
data Free t a
        = Pure a
        | Join (t (Free t a))

type Coyoneda t a = forall x . (tx, x -> a)
```

このようにする(これを、実際にGHCに読み込むにはRankNTypes拡張が必要)。
すると、つぎのようになる。

```hs
Free (Coyoneda t) a
        = Pure a
        | Join (Coyoneda t (Free (Coyoneda t) a))
```

展開する。

```hs
Free (Coyoneda t) a
        = Pure a
        | forall x . Join (t x, x -> Free (Coyoneda t) a)
```

Free (Coyoneda t)を、FreeCoyoneda tのようにまとめてしまおう。

```hs
FreeCoyoneda t a
        = Pure a
        | forall x . Join (t x, x -> FreeCoyoneda t a)
```

値構築子Joinが、タプルではなく、ふたつの引数をとるようにする。

```hs
FreeCoyoneda t a
        = Pure a
        | forall x . Join (t x) (x -> FreeCoyoneda t a)
```

FreeCoyonedaではなく、Freerに名前を変える。
また、値構求子Joinの名前もBindに変更しよう。
ファイルFreer.hsを作成する。

```hs:Freer.hs
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer where

import Control.Monad ((>=>))

data Freer t a
        = Pure a
        | forall x . Bind (t x) (x -> Freer t a)
```

データ型Freerが、FreeモナドとCoyonedaとを組み合わせて、ひとつにしたデータ型だ。
あとで演算子(>=>)を使うので、ここでControl.Monadを導入しておいた。
値構築子Bindの型をみてみよう。

```hs
> :load Freer.hs
> :type Bind
Bind :: t x -> (x -> Freer t a) -> Freer t a
> :type (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

値構築子Bindの型はバインド演算子(>>=)によく似ている。

Monadクラスのインスタンスにする
-------------------------------

### Monadクラスのインスタンス宣言

Monadクラスのインスタンスにするには、
Functor、Applicativeの両クラスのインスタンスにする必要がある。
後二者のインスタンス宣言は、とりあえず、スタブとしておこう。
ただし、Applicativeクラスのpureはreturnとおなじなので、
それだけは、ここで定義しておく。

```hs:Freer.hs
instance Functor (Freer t) where

instance Applicative (Freer t) where
        pure = Pure

instance Monad (Freer t) where
        Pure x >>= f = f x
	Bind tx k >>= f = Bind tx $ k >=> f
```

クラス関数return(関数pureとして定義)は、値構築子Pureそのものである。
バインド演算子(>>=)は、第1引数が値構築子Pure xならば、
(f :: a -> Freer t b)をそのまま値xに適用すればいい。
Bind tx kのときには、演算子(>=>)で関数kとfとを合成すればいい。

### Functor、Applicativeクラスのインスタンス宣言

Functor、Applicativeクラスのクラス関数である関数fmap、演算子(<\*>)については、
つぎの関係から定義することができる。

```hs
fmap f = (>>= return . f)
mf <*> mx = mf >>= \f -> mx >>= \x -> return $ f x
```

これに、あてはめて変形していけばいい。
ファイルFreer.hsの、それぞれのインスタンス宣言の、なかみを定義する。

```hs:Freer.hs
instance Functor (Freer t) where
        f `fmap` Pure x = Pure $ f x
        f `fmap` Bind tx k = Bind tx $ k >=> Pure . f

instance Applicative (Freer t) where
        pure = Pure
        Pure f <*> m = f <$> m
        Bind tx k <*> m = Join tx $ k >=> (<$> m)
```

Readerモナド
------------

Writerモナド
------------

状態モナド
----------

エラーモナド
------------
