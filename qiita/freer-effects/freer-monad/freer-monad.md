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

文脈つきの値をFreerモナドに変換する関数を作成する。
ファイルFreer.hsに関数freerの定義を追加する。

```hs:Freer.hs
freer :: t a -> Freer t a
freer = (`Bind` Pure)
```

Readerモナド
------------

おなじみのReaderモナドを定義してみよう。
ファイルreader.hsを作る。

```hs:reader.hs
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

data Reader e a where
        Reader :: Reader e e

ask :: Freer (Reader e) e
ask = freer Reader

runReader :: Freer (Reader e) a -> e -> a
runReader m e = case m of
        Pure x -> x
        Bind Reader k -> runReader (k e) e
```

FreeモナドとCoyonedaの組み合わせを説明したときの定義と、
ほとんど、おなじだ。
データ型Coyonedaを作っていないぶん、すこしシンプルになる。
試してみよう。

```hs
> :load reader.hs
> :module + Data.Char
> (`runReader` 100) $ do x <- ask; return $ chr x
'd'
```

Writerモナド
------------

こちらも、おなじみのWriterモナドを定義しよう。

```hs:writer.hs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid

import Freer

data Writer w a where
        writer :: w -> Writer w ()

tell :: w -> Freer (Writer w) ()
tell = freer . Writer

runWriter :: Monoid w => Freer (Writer w) a -> (a, w)
runWriter = \case
        Pure x -> (x, mempty)
        Bind (Writer w) k -> second (w <>) . runWriter $ k ()
```

こちらも、FreeモナドとCoyonedaの組み合わせのところで説明したWriterモナドと、
ほぼ、おなじものだ。
やはり、すこしシンプルになる。
例として、つぎの値を定義してみよう。

```hs:writer.hs
set :: String -> Integer -> Freer (Writer String) Integer
set var val = do
        tell $ var ++ " = " ++ show val ++ "\n"
	return val

sample :: Freer (Writer String) Integer
sample = do
        x <- set "x" 8
        y <- set "y" 5
        tell $ "x * y = " ++ show (x * y) ++ "\n"
	return $ x * y
```

対話環境で試してみよう。

```hs
> :load writer.hs
> runWriter sample
(40,"x = 8\ny = 5\nx * y = 40\n")
```

状態モナド
----------

エラーモナド
------------
