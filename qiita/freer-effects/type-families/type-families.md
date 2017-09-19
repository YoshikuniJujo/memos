Freer Effectsが、だいたいわかった: 3. 型族(TypeFamilies拡張)の解説
=================================================================

目次
----

0. [導入](../prelude.md)

1. [Freeモナドの概要](../free-monad/free-monad.md)
	* Freeモナドとは
	* FreeモナドでReaderモナド、Writerモナドを構成する
2. [存在型(ExistentialQuantification拡張)の解説](
	../existential-quantification/existentials.md )
4. 一般化代数データ型(GADTs拡張)の解説
5. FreeモナドとCoyoneda
	* Coyonedaを使ってみる
	* FreeモナドとCoyonedaを組み合わせる
		+ いろいろなモナドを構成する
6. Freerモナド(Operationalモナド)でいろいろなモナドを構成する
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
7. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
8. 存在型による拡張可能なデータ構造(Open Union)
9. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
10. Open Unionを型によって安全にする
11. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
12. 関数を保管しておくデータ構造による効率化
13. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

はじめに
--------

Freer Effectsは、型族(TypeFamilies拡張)を使用せずに構成できる。
ここで、型族を解説するのは、Freer Effectsで使用する
一般化代数データ型(GADTs拡張)を説明するうえで、
型族を導入としたほうが、わかりやすいと考えたからだ。

型シノニム族
------------

### 型シノニム族とは

型シノニムの定義って何かに似てる。
なんとなく関数定義と似てる気がする。
型シノニムの例として、つぎのような定義をみてみよう。

```hs
type Foo x = Either x Bool
```

これと、つぎのような、関数定義の例とをくらべてみる。

```hs
foo x = mod x 8
```

似ている。
また、これを、それぞれ、型や値に適用する。

```hs
Foo Integer ==> Either Integer Bool
foo 123 ==> mod 123 8
```

これも、また似ている。
型シノニムの定義は、型に対する関数を定義していると考えられる。
さて、関数の仮引数としてリテラルを指定することができる。

```hs
foo 4492 = 2944
bar "hello" = 1234
```

おなじことを型シノニムでもやりたいとする。
Foo IntegerはBoolに、Foo DoubleはCharにしたいとしよう。
つぎのように、なるだろうか。

```hs
type Foo Integer = Bool
type Foo Double = Char
```

標準的なHaskellでは、このように型シノニムの仮引数に具体的な型の名前を
置くことはできない。
GHCではTypeFamilies拡張を使い、型Fooを明示的に型族としてやれば、
上記のような定義が可能になる。
つぎのように、ファイルtypeFamily.hsを作成する。

```hs:typeFamily.hs
{-# LANGUAGE TypeFamilies #-}

type family Foo x

type instance Foo Integer = Bool
type instance Foo Double = Char
```

さて、値に型(type)があるように、型には種類(kind)がある。
そのまま使える型Int, Double, Charなどの種類は\*であり、
型引数をひとつとる型Maybeや[]などの種類は\* -> \*であり、
型引数をふたつとる型Eitherや(,)などの種類は\* -> \* -> \*である。
型Fooの種類を、みてみよう。

```hs
> :load typeFamily.hs
> :kind Foo
Foo :: * -> *
> :kind Foo Integer
Foo Integer :: *
```

この:kindというコマンドに、!をつけた:kind!というコマンドがある。
これを使うと、型シノニムを正規化された型に変換して、表示してくれる。

```hs
> :kind! Foo Integer
Foo Integer :: *
= Bool
> :kind! Foo Double
Foo Double :: *
= Char
```

型シノニム族を使うと、関数定義でリテラルを仮引数とするのとおなじように、
型シノニムの定義で型仮引数に、型変数ではなく、具体的な型の名前を使うことが
できるようになる。

### 型シノニム族を使う

型シノニム族は、ある型と別の型とを関連づけたいときに使える。
たとえば、リストをその要素の型によって、
より空間効率のいい構造に変換することを考える。
たとえばユニット型の値のリストは、その長さを表す整数に置き換えることができる。
また、真偽値型の値のリストは、
長さを表す整数と、それぞれのビットのオン/オフを表す整数に置き換えることができる。
8ビット非負整数のリストはByteString型の値に置き換えられる。
またDouble型の値のリストは、そのままのリストとすることにする。

もとのリストの要素の型と、それの空間効率のいい表現との対応を示す。

```hs
() ==> Int
Bool ==> (Int, Integer)
Word8 ==> Data.ByteString.ByteString
Double ==> [Double]
```

ファイルpackable.hsを作成し、型族を使って、この対応を表現してみよう。
必要なモジュールの導入などの記述も、あらかじめしておこう。

```hs:packable.hs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Bits
import Data.Bool
import Data.Word

import qualified Data.ByteString as BS

type family List x

type instance List () = Int
type instance List Bool = (Int, Integer)
type instance List Word8 = BS.ByteString
type instance List Double = [Double]
```

さて、もとのリストの要素の型と、対応する構造との組を定義することができた。
つぎにしたいことは、当然、リストからの変換とリストへの変換になるだろう。
それらの型は、つぎのようになるはずだ。

```hs
fromList :: [a] -> List a
toList :: List a -> [a]
```

これらの関数は、型変数aの型によって、定義のしかたが変わる。
よって、型クラスのクラス関数にする必要がある。
ファイルpackable.hsに、つぎのように追加しよう。

```hs:packable.hs
class Packable p where
        fromList :: [p] -> List p
        toList :: List p -> [p]
```

それぞれの型に対する関数fromList, toListを定義する。

```hs:packable.hs
instance Packable () where
        fromList = length
        toList = (`replicate` ())

instance Packable Bool where
        fromList = \case
                [] -> (0, 0)
                b : bs -> (+ 1) *** (bool 0 1 b .|.) . (`shiftL` 1)
                        $ fromList bs
        toList (l, n) | l <= 0 || n < 0 = []
        toList (l, n) = n `testBit` 0 : toList (l - 1, n `shiftR` 1)

instance Packable Word8 where
        fromList = BS.pack
        toList = BS.unpack

instance Packable Double where
        fromList = id
        toList = id
```

試してみよう。

```hs
> :load packable.hs
> fromList [(), (), (), (), ()]
5
> toList it :: [()]
[(),(),(),(),()]
> fromList [True, False, False, True, False]
(5,9)
> toList it :: [Bool]
[True,False,False,True,False]
> fromList [100 :: Word8, 123, 115, 120, 125]
"d{sx}"
> toList it :: [Word8]
[100,123,115,120,125]
> fromList [3.5 :: Double, 8.2, 1.3]
[3.5,8.2,1.3]
> toList it :: [Double]
[3.5,8.2,1.3]
```

### 型クラスと関連づけられた型シノニム族

### 閉じた型シノニム族

データ族
--------

### データ族とは

### データ族を使う

### 閉じたデータ族?

参考
----

[Wikibooks: GHC/Type families](https://wiki.haskell.org/GHC/Type_families)
