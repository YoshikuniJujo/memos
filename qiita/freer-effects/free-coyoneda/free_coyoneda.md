Freer Effectsが、だいたいわかった: 6. FreeモナドとCoyoneda
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
7. FreeモナドとCoyoneda
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

Coyoneda
--------

### Coyonedaとは

Coyonedaとは、引数をひとつとる型をラップして、ファンクタに変換することのできる
データ型だ。
定義は、つぎのようになる。
ファイルCoyoneda.hsを作成する。

```hs:Coyoneda.hs
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Coyoneda where

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)

coyoneda :: t a -> Coyoneda t a
coyoneda = (`Coyoneda` id)

instance Functor (Coyoneda t) where
        fmap f (Coyoneda tx g) = Coyoneda tx $ f . g
```

関数coyonedaによって、もともとの値をラップすることで、
関数fmapによる「なかの値への関数適用」ができるようになる。
Coyonedaのしていることは、もともとの文脈つきの値と、
適用される関数とを別々に保存しているだけの話だ。

### 名前について

Coyonedaって変な名前だ。
どうしてこんな名前なのだろう。
よくわからないけど、「米田の補題」という圏論の理論([Wikipedia: 米田の補題](
	https://ja.wikipedia.org/wiki/米田の補題))に関係があるようだ。
でも、「米田の補題」は知らなくてもCoyonedaは使えるので、ご心配なく。

### 試してみたい

対話環境で「さくっ」と試してみたいけれど、
型CoyonedaはShowクラスのインスタンスではないのでムリ。
とりあえず、つぎのような、
「もともとファンクタであるようなデータ型を、Coyonedaでつつんで、
関数fmapで関数適用したうえで、もとの型にもどす」という、
例で試してみよう。
ファイルCoyoneda.hsに定義を追加する。

```hs:Coyoneda.hs
adenoyoc :: Functor f => Coyoneda f a -> f a
adenoyoc (Coyoneda fx f) = f <$> fx
```

対話環境で試してみる。

```hs
> :load Coyoneda.hs
> adenoyoc . ((* 15) <$>) . coyoneda $ Just 32
Just 480
> adenoyoc . ((* 15) <$>) $ coyoneda [3, 4, 5]
[45,60,75]
> adenoyoc . (reverse <$>) $ coyoneda getLine
(helloと入力し改行)hello
"olleh"
```

ちゃんと定義できているのがわかる。

### 文脈への関数適用

Coyoneda型の値の文脈に対して、関数適用できるようにする。

```hs:Coyoneda
fromContext :: (forall x . t x -> b) -> Coyoneda t a -> b
fromContext f (Coyoneda tx _) = f tx

mapContext :: (forall x . t x -> t' x) -> Coyoneda t a -> Coyoneda t' a
mapcontext f (Coyoneda tx g) = Coyoneda (f tx) g
```

関数fromContextは、文脈に対して関数を適用して、単純な値をかえす。
関数mapContextは、文脈に対して関数を適用して、新しいCoyoneda型の値をかえす。
試してみよう。

```hs
> :reload
> :module + Data.Char
> fromContext null $ Coyoneda [100 .. 110] chr
False
> :module + Data.Maybe
> adenoyoc . mapContext listToMaybe $ Coyoneda [100 .. 110] chr
Just 'd'
```

### もうすこし意味のある例

もうすこしだけ意味のある例をみる。
まずは、Maybe型と、だいたい、おなじだけど、なかの値を正格評価するStrictMaybe型を
つくる。
つぎのような、ファイルstrict.hsを作成する。

```hs:strict.hs
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Concurrent
import System.IO.Unsafe

import Coyoneda

data StrictMaybe a = StrictNothing | StrictJust !a deriving Show

isStrictJust :: StrictMaybe a -> Bool
isStrictJust StrictNothing = False
isStrictJust (StrictJust _) = True
```

StrictJust !aの"!"に注目。
これはStrictMaybe型の値を評価するときに、
値構築子StrictJustの引数をさきに評価するようにする、しるしだ。
関数isStrictJustはStrictMaybe型の値が「値を持つ(StrictJust xである)」ことを
確認する。

このStrictJust型の値をCoyoneda型の値に変換することで、
遅延評価をエミュレートする例を示そうと思う。
遅延評価であることを実感するために、「遅い処理」のモデルを作る。

```hs:strict.hs
slowSucc :: Integer -> Integer
slowSucc n = unsafePerformIO $ threadDelay 1000000 >> return (succ n)
```

ここで、StrictMaybe型の値のリストの要素が、すべてStrictJust値であるときだけ、
先解の値をかえす関数をかんがえる。

```hs:strict.hs
headIfAllJust :: [StrictMaybe a] -> StrictMaybe a
headIfAllJust mms@(m : _) | all isStrictJust ms = m
headIfAllJust _ = StrtictNothing
```

おなじことをCoyoneda型の値に変換してから、おこなう関数を定義する。

```hs:strict.hs
headIfAllJustCoyoneda :: [Coyoneda StrictMaybe a] -> Coyoneda StrictMaybe a
headIfAllJustCoyoneda mms@(m : _) | all (fromContext isStrictJust) mms = m
headIfAllJustCoyoneda _ = coyoneda StrictNothing
```

対話環境で試してみよう。

```hs
> :load strict.hs
> headIfAllJust $ fmap slowSucc . StrictJust <$> [3 .. 8]
StrictJust 4
> adenoyoc . headIfAllJustCoyoneda $ fmap slowSucc . coyoneda . StrictJust <$> [3 .. 8]
StrictJust 4
```

Coyoneda型の値に変換しないで評価したほうでは、評価に6秒かかる。
Coyoneda型の値に変換したほうでは評価に1秒しかかからない。

FreeモナドとCoyonedaを組み合わせる
----------------------------------

Coyoneda型を利用する例を、ムリヤリ、作ってみたが、
やはりCoyonedaはFreeモナドと組み合わせてこそ、
本当の力を発揮することができる。

もういちど、Freeモナドの定義をみてみよう。

```hs:Free.hs
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Free where

data Free t a
        = Pure a
        | Join (t (Free t a))

instance Functor t => Functor (Free t) where
        f `fmap` Pure x = Pure $ f x
        f `fmap` Join tx = Join $ fmap f <$> tx

instance Functor t => Applicative (Free t) where
        pure = Pure
        Pure f <*> m = f <$> m
        Join tf <*> m = Join $ (<*> m) <$> tf

instance Functor t => Monad (Free t) where
        Pure x >>= f = f x
        Join tx >>= f = Join $ (f =<<) <$> tx
```

### ファンクタならFreeでモナドになる

ファンクタならFreeでモナドになる。
Coyonedaなら、引数をとる型をファンクタにできる。
つまりFreeとCoyonedaを組み合わせれば、引数をとる型なら何でもモナドになる。
つぎのようにモジュールFreeCoyonedaを作成する。

```hs:FreeCoyoneda.hs
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module FreeCoyoneda (module Free, module Coyoneda, toFC) where

import Free
import Coyoneda

toFC :: t a -> Free (Coyoneda t) a
toFC = Join . (Pure <$>) . coyoneda
```

toFCによって、(t a)のようにあらわされる型の値を、
Free (Coyoneda t) aのような型に変換することができる。
これはモナドになる。

### MyMaybeモナド

Maybeモナドと同等の型を作成し、それをFreeとCoyonedaでモナドに変換してみよう。
つぎのようなファイルmaybe.hsを作成する。

```hs:maybe.hs
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import FreeCoyoneda

data MyMaybe a = MyNothing | MyJust a deriving Show

checkPositive :: Integer -> MyMaybe Integer
checkPositive n
        | n > 0 = MyJust n
        | otherwise = MyNothing

sample :: Integer -> Free (Coyoneda MyMaybe) Integer
sample n = do
        m <- toFC . checkPositive $ n - 9
        return $ m + 5

runMyMaybe :: Free (Coyoneda MyMaybe) a -> MyMaybe a
runMyMaybe = \case
        Pure x -> MyJust x
        Join (Coyoneda MyNothing _) -> MyNothing
        Join (Coyoneda (MyJust x) k) -> runMyMaybe $ k x
```

このように、型MyMaybeそのものをMonadクラスのインスタンスにしなくても、
Free (Coyoneda MyMaybe)のように変換することでモナドとすることができる。
対話環境で試してみよう。

```
> :load maybe.hs
> runMyMaybe $ sample 8
MyNothing
> runMyMaybe $ sample 15
MyJust 11
```

### Readerモナド

Readerモナドを作ってみよう。
つぎのようなファイルreader.hsを作成する。

```hs:reader.hs
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import FreeCoyoneda

data Reader e a where
        Reader :: Reader e e

ask :: Free (Coyoneda (Reader e)) e
ask = toFC Reader

runReader :: Free (Coyoneda (Reader e)) a -> e -> a
runReader m e = case m of
        Pure x -> x
        Join (Coyoneda Reader k) -> runReader (k e) e
```

対話環境で試してみよう。

```hs
> :load reader.hs
> :module + Data.Char
> (`runReader` 100) $ do x <- ask; return $ chr x
'd'
```

Reader型には、引数をとらない値構築子Readerだけがある。
また値構築子ReaderはGADTによってReader e eのように、
型引数の、ひとつめとふたつめとが、おなじであるように限定されている。
これによって、Coyoneda Reader k :: Coyoneda (Reader e) aにおいて、
kの型が(e -> a)となることが保証される。

### Writerモナド

Writerモナドを作ってみよう。
つぎのようなファイルwriter.hsを作成する。

```hs:writer.hs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid

import FreeCoyoneda

data Writer w a where
        Writer :: w -> Writer w ()

tell :: w -> Free (Coyoneda (Writer w)) ()
tell = toFC . Writer

runWriter :: Monoid w => Free (Coyoneda (Writer w)) a -> (a, w)
runWriter = \case
        Pure x -> (x, mempty)
        Join (Coyoneda (Writer w) k) -> second (w <>) . runWriter $ k ()

sample :: Free (Coyoneda (Writer String)) Integer
sample = do
        tell "x = 8\n"
        x <- return 8
        tell "y = 5\n"
        y <- return 5
        tell $ "x * y = " ++ show (x * y) ++ "\n"
        return $ x * y
```

対話環境で試してみる。

```hs
> :load writer.hs
> runWriter sample
(40,"x = 8\ny = 5\nx * y = 40\n")
```

Writer型は、ログの記録のための型wを引数とする値構築子Writerをもつ。
この値構築子の型はWriter w ()となる。
値(Coyoneda Writer k :: Coyoneda (Writer w) a)における関数kの型は、
(() -> a)となる。
Writer型の本質であるw型の値の保持と、モナドの返り値であるa型の値とが、
Writer型とCoyoneda型に分離されているのがわかる。

### ReaderとWriterモナドを組み合わせる

ReaderとWriterモナドを組み合わせたモナドを作る。
うえでみたReaderモナドとWriterモナドとを合わせた型を作る。

```hs:readerWriter
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid

import FreeCoyoneda

data RW e w a where
        Reader :: RW e w e
        Writer :: w -> RW e w ()

ask :: Free (Coyoneda (RW e w)) ()
ask = toFC Reader

tell :: w -> Free (Coyoneda (RW e w)) ()
tell = toFC . Writer

sample :: Free (Coyoneda (RW String String)) (String, String)
sample = do
        x <- ask
        tell $ "I say " ++ x ++ ".\n"
	y <- ask
	tell $ "You say Good-bye!\n"
	return (x, y)
```

#### もともとの意味論での評価

Readerモナド、Writerモナドのそれぞれの意味論で評価する関数runRWを定義する。

```hs:readerWriter.hs
runRW :: Monoid w => Free (Coyoneda (RW e w)) a -> e -> (a, w)
runRW m e = case m of
        Pure x -> (x, mempty)
	Join (Coyoneda Reader k) -> runRW (k e) e
	Join (Coyoneda (Writer w) k) -> second (w <>) $ runRW (k ()) e
```

対話環境で試してみる。

```hs
> :load readerWriter.hs
> sample `runRW` "hello"
(("hello","hello"),"I say hello.\nYou say Good-bye!\n")
```

#### 状態モナドとしての評価

ReaderモナドとWriterモナドの機能を合わせて、状態モナドとして評価する。

```hs:readerWriter.hs
runStateRW :: Free (Coyoneda (RW s s)) a -> s -> (a, s)
runStateRW m s = case m of
        Pure x -> (x, s)
        Join (Coyoneda Reader k) -> runStateRW (k s) s
	Join (Coyoneda (Writer s') k) -> runStateRW (k ()) s'
```

対話環境で試してみる。

```hs
> :reload
> sample `runStateRW` "hello"
(("hello","I say hello.\n"),"You say Good-bye!\n")
```

まとめ
------

引数をとる型をファンクターに変換することのできるデータ型Coyonedaを、
定義して使ってみた。
やっていることは、値と、それに適用する関数とを、別々に保存しているだけだ。
Coyonedaを使う例として、正格評価されるデータ型を、
遅延評価するようにする例を挙げた。
またFreeモナドと組み合わせて使うことで、
新しいモナドの作成がより効率的にできる
(Functorクラスのインスタンスにする手間がはぶける)ことを示した。
ReaderモナドとWriterモナドの例は、Freeモナドでの例を、Coyonedaを追加して
書き直したものだ。

参考
----

* [モナドとわたしとコモナド: Freeモナドを超えた!?operationalモナドを使ってみよう](
	http://fumieval.hatenablog.com/entry/2013/05/09/223604 )
* [YonedaとCoYoneda、そしてFunctor](
	http://d.hatena.ne.jp/its_out_of_tune/20130601/1370109743 )
