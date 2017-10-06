Freer Effectsが、だいたいわかった: 9. モナドを混ぜ合わせる(閉じた型で)
======================================================================

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
9. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
		+ 両方のモナドを一度に処理する
		+ それぞれのモナドを、それぞれに処理する
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

状態モナドとエラーモナドとを混ぜ合わせる
----------------------------------------

Freerモナドを使って、状態モナドとエラーモナドとを混ぜ合わせてみる。
ファイルstateError.hsを作成する。

```hs:stateError.hs
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

data SE s e a where
        Get :: SE s e s
        Put :: s -> SE s e ()
        Exc :: e -> SE es e a
```

状態モナドでのデータ型の値構築子と、
エラーモナドでのデータ型の値構築子との両方をもつデータ型を定義した。
それぞれに対して、モナドを構成する基本的な要素を定義する。

```hs:stateError.hs
get :: Freer (SE s e) s
get = freer Get

put :: s -> Freer (SE s e) ()
put = freer . Put

modify :: (s -> s) -> Freer (SE s e) ()
modify f = put . f =<< get

throwError :: e -> Freer (SE s e) a
throwError = freer . Exc
```

状態モナドとエラーモナドを、一度に処理する
------------------------------------------

状態+エラーモナドを処理する関数を書く。
状態モナドとエラーモナドを混ぜ合わせるとき、
エラーのときに状態を捨てるかどうかで、2通りの関数が考えられる。

### エラーのとき状態を捨てる

エラーのとき状態を捨てる処理をする、関数を書く。

```hs:stateError.hs
runSE :: Freer (SE s e) a -> s -> Either e (a, s)
runSE m s = case m of
        Pure x -> Right (x, s)
        Get `Bind` k -> runSE (k s) s
        Put s' `Bind` k -> runSE (k ()) s'
        Exc e `Bind` _k -> Left e
```

まえにみた関数runState、runErrorとを混ぜ合わせたような関数となっている。

### サンプル

とくに意味はないが、計算の例を示す。
まずは安全な除算を定義する。

```hs:stateError.hs
safeDiv :: Integer -> Integer -> Freer (SE s String) Integer
n `safeDiv` 0 = throwError $ show n ++ " is divided by 0"
n `safeDiv` m = return $ n `div` m
```

これを使って計算の流れを定義する。

```hs:stateError.hs
sample1 :: Freer (SE Integer String) Integer
sample1 = do
        a <- get
        modify (subtract 5)
        modify (* 2)
        b <- get
        c <- 60 `safeDiv` b
        put a
        modify (subtract 3)
        d <- get
        e <- 250 `safeDiv` d
        return $ c + e
```

とくに意味はないが、つぎのような計算をしている。
状態として受け渡されていく値を「メモリの値」と呼ぶことにする。

* はじめのメモリの値を変数aに読み込む
* メモリの値から5をひく
* メモリの値に2をかける
* メモリの値を変数bに読み込む
* 60を値bでわり(値bが0ならばエラーとなる)、結果を変数cに代入する
* 値aをメモリの値に代入する
* メモリの値から3をひく
* メモリの値を変数dに読み込む
* 250を値dでわり(値dが0ならばエラーとなる)、結果を変数eに代入する
* 値cと値eの和をかえす

対話環境で試してみよう。

```hs
> :load stateError.hs
> sample1 `runSE` 8
Right (60,5)
> sample1 `runSE` 5
Left "60 is divided by 0"
> sample1 `runSE` 3
Left "250 is divided by 0"
```

### エラーでも状態を保持する

### エラーからの復帰

### エラーからの復帰を含むサンプル

状態モナドとエラーモナドとを、それぞれに処理する
------------------------------------------------

共通するかたち
--------------

さらに...を追加する
-------------------

拡張性がない
------------

データ型に新しい値構築子を追加して、
それに対するrun...を定義すれば、新しい機能が追加できる。
run...は別モジュールで追加することもできるけれど、
Haskellのデータ型は閉じているので、
別モジュールで値構築子を追加することはできない。
はじめに定義したデータ型に含まれた機能以上のことはできない。
それなら、値構築子を追加できるデータ型を作ればいいのでは。

まとめ
------