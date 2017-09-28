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
6. FreeモナドとCoyoneda
	* Coyonedaを使ってみる
	* FreeモナドとCoyonedaを組み合わせる
		+ いろいろなモナドを構成する
7. Freerモナド(Operationalモナド)でいろいろなモナドを構成する
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
8. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
9. 存在型による拡張可能なデータ構造(Open Union)
10. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
11. Open Unionを型によって安全にする
12. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
13. 関数を保管しておくデータ構造による効率化
14. いろいろなEffect
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
{-# LANGUAGE ExistentialQuantification #-}
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
あまり意味のない例で試してみよう。
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

FreeモナドとCoyonedaを組み合わせる
----------------------------------
