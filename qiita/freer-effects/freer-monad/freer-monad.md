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
        Reader `Bind` k -> runReader (k e) e
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
        Writer w `Bind` k -> second (w <>) . runWriter $ k ()
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

状態モナドを定義する。
まえにReaderモナドとWriterモナドを、まぜて状態モナドとして解釈したことがあるが、
だいたいおなじことをすればいい。
ファイルstate.hsを作成する。

```hs:state.hs
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

data State s a where
        Get :: State s s
        Put :: s -> State s ()

get :: Freer (State s) s
get = freer Get

put :: s -> Freer (State s) ()
put = freer . Put

modify :: (s -> s) -> Freer (State s) ()
modify f = put . f =<< get

runState :: Freer (State s) a -> s -> (a, s)
runState m s = case m of
        Pure x -> (x, s)
        Get `Bind` k -> runState (k s) s
        Put s' `Bind` k -> runState (k ()) s'
```

Getがきたら、そのときの状態sを関数kにわたす。
式k sはFreer (State s) a型の値に評価され、再帰的に関数runStateが適用される。
Put s'がきたら、状態をs'に更新してから、つづきのモナドとなるk ()に、
関数runStateを適用する。
モナドsampleの例をみてみよう。
ファイルstate.hsに追加する。

```hs:state.hs
sample :: Freer (State Integer) Integer
sample = do
        modify (+ 8)
        modify (* 5)
        s <- get
        put 4
	modify (+ 3)
        s' <- get
        return $ s + s'
```

対話環境で試してみる。

```hs
> :load state.hs
> sample `runState` 4
(67,7)
> sample `runState` 2
(57,7)
```

エラーモナド
------------

エラーモナドを定義する。
ファイルexception.hsを作る。

```hs:exception.hs
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

newtype Exc e a = Exc e

throwError :: e -> Freer (Exc e) a
throwerror = freer . Exc

runError :: Freer (Exc e) a -> Either e a
runError = \case
        Pure x -> Right x
        Exc e `Bind` _k -> Left e
```

値Exc eがあれば、それ以降の処理(\_k)はおこなわれずに、Left eに評価される。
0除算をエラーとする関数safeDivを定義する。

```hs:exception.hs
safeDiv :: Integer -> Integer -> Freer (Exc String) Integer
safeDiv n 0 = throwError $ show n ++ " is divided by 0"
safeDiv n m = return $ n `div` m
```

対話環境で試してみる。

```hs
> runError $ do x <- 30 `safeDiv` 6; y <- 45 `safeDiv` 3; return $ x + y
Right 20
> runError $ do x <- 30 `safeDiv` 6; y <- 45 `safeDiv` 0; return $ x + y
Left "45 is divided by 0"
```

### エラーからの復帰

エラーから復帰する仕組みも作ろう。
ファイルexception.hsに関数catchErrorを定義する。

```hs:exception.hs
catchError :: Freer (Exc e) a -> (e -> Freer (Exc e) a) -> Freer (Exc e) a
m `catchError` h = case m of
        Pure x -> return x
        Bind (Exc e) _k -> h e
```

値Exc eがあったときにはエラーハンドラー関数hをエラー値eに適用する。
サンプルを定義する。

```hs:exception.hs
sample :: Integer -> Integer -> Freer (Exc String) Integer
sample n m = do
        a <- 50 `safeDiv` n
        b <- (100 `safeDiv` m) `catchError` const (return 50000)
        return $ a + b
```

対話環境で試してみよう。

```hs
> runError $ sample 5 10
Right 20
> runError $ sample 0 10
Left "50 is divided by 0"
> runError $ sample 5 0
Right 50010
```

まとめ
------

Freerモナドを使って、Reader、Writer、状態、エラーの、それぞれのモナドを作った。
「モナドらしい」部分がデータ型Freerに切り出されているために、
それぞれに定義されるデータ型は、それぞれのモナドの本質的な中核部分のみを
定義するだけですんでいる。
