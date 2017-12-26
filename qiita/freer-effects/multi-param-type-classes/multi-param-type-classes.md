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
	4. [DataKinds拡張](
		../data-kinds/data-kinds.md )
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

プレーンなHaskellでは、型クラスのとれる引数は、ひとつである。
言語拡張を使うことで、型クラスが複数の引数をとれるようにできる。

Ruby的かけ算
------------

### 何がしたいか

Rubyでは、文字列と数値とをかけあわせることができる。
もちろん、数値と数値とをかけあわせることもできる。
これを実現するために、かけ算の第1引数と第2引数とを、別の型にできるような、
多相的なかけ算を定義してみよう。

### コードと実行例

つぎのような、ファイルmul.hsを作成する。

```hs:mul.hs
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding ((*))
import qualified Prelude
import qualified Data.ByteString as BS

class Mulable a b where
        (*) :: a -> b -> a
```

かけ算の結果の値の型は、第1引数の型とおなじ型とする。
「文字列 * 整数」についてインスタンスを定義する。

```hs:mul.hs
instance Mulable BS.ByteString Int where
        bs * n = BS.concat $ replicate n bs

instance Mulable BS.ByteString Integer where
        bs * n = BS.concat $ replicate (fromInteger n) bs
```

計算例を定義する。

```hs:mul.hs
threeHello :: BS.ByteString
threeHello = "Hello" * (3 :: Int)

sevenHello :: BS.ByteString
sevenHello = "Hello" * (7 :: Int)
```

対話環境でみてみよう。

```hs
> :load mul.hs
> threeHello
"HelloHelloHello"
> sevenHello
"HelloHelloHelloHelloHelloHelloHello"
```

「Int型の値 * 整数」についてインスタンス定義する。

```hs:mul.hs
instance Mulable Int Int where
        n * m = n Prelude.* m

instance Mulable Int Integer where
        n * m = n Prelude.* fromInteger m
```

計算例を定義する。

```hs:mul.hs
threeFive :: Int
threeFive = 5 * (3 :: Int)

sevenFive :: Int
sevenFive = 5 * (7 :: Integer)
```

対話環境でみてみる。

```hs
> :reload
> threeFive
15
> sevenFive
35
```

もりあがってきたので、
「文字列 * 浮動小数点数」や「Int型の値 * 浮動小数点数」も定義する。

```hs:mul.hs
instance Mulable BS.ByteString Double where
        bs * x = BS.take l . BS.concat $ replicate (ceiling x) bs
	        where l = round $ fromIntegral (BS.length bs) prelude.* x

instance Mulable Int Double where
        n * x = round $ fromIntegral n Prelude.* x

threePointFourHello :: BS.ByteString
threePointFourHello = "Hello" * (3.4 :: Double)

threePointFourFive :: Int
threePointFourFive = 5 * (3.4 :: Double)
```

試してみよう。

```hs
> :reload
> threePointFourHello
"HelloHelloHelloHe"
> threePointFourFive
17
```

### 設計のまずさ

わかりやすい例として挙げたけれど、上のような設計をするのは、
あまりかしこいとは言えない。
かけ算の第1引数となる型の数nと、第2引数となる型の数mに対して、
n * m個のインスタンス宣言が必要になる。

かしこい設計は、おそらく、「いちど共通の型に変換して云々」といったところか。
プログラミングの名著中の名著である
SICP(Structure and Interpretation of Computer Programs)
あたりで触れられていたように思う。

まとめ
------

MultiParamTypeClasses拡張を使うと、
型クラスが複数の引数をとることができるようになる。
わかりやすい例として「Rubyっぽいかけ算」の例を挙げた。
この例は「わかりやすい」けれど、「かしこくない設計」ではある。
