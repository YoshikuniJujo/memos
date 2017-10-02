Freer Effectsが、だいたいわかった: 6. ランクN多相(RankNTypes拡張)の解説
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
6. ランクN多相(RankNTypes拡張)の解説
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

ランク1多相(rank-1 type)
------------------------

標準的なHaskellのパラメトリック多相関数について考える。
たとえば、つぎのような関数がある。

```hs
const :: a -> b -> a
```

この型変数a, bにはどのような型でもいれることができる
(実はGHCに関しては厳密にはちがうのだが、ここでは気にしないでほしい)。
このように、Haskellで使われる「ふつうの」多相関数はランク1多相関数である。

ランク2多相(rank-2 type)
------------------------

### うまくいかない

つぎのコードをみてみよう。

```hs:bad.hs
fun :: ([a] -> [a]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun f ns cs = (f ns, f cs)
```

たとえば、fun reverse [1, 2, 3] "hello"のようにすると、
([3, 2, 1], "olleh")に評価される、とふつうは思うだろう。
しかし、この関数funは定義できない。

### 多相関数を引数にとる関数は定義できない

標準的なHaskellでは、多相関数を引数にとる関数を定義することはできない。
うえで定義しようとした関数funは、つぎのような関数を同時に定義しようとしていた。

```hs
fun1 :: ([Int] -> [Int]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun2 :: ([Double] -> [Double]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun3 :: ([Integer] -> [Integer]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun4 :: ([Char] -> [Char]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun5 :: ([Bool] -> [Bool]) -> [Integer] -> [Char] -> ([Integer], [Char])
.
.
.
```

このように、みていくと、みるからにダメそうであることがわかる。

### それは実はちがう

「多相関数を引数にとる関数が定義できない」だって!?
そんなバカな。
僕らのアイドル、関数mapがあるじゃないか!!

```hs
map :: (a -> b) -> [a] -> [b]
```

(a -> b)型の多相関数を引数にとってるよね!!!
まあまあ、落ち着いてください。
関数mapは、多相関数を引数にとっているのではない。
うえの例とおなじように考えてみる。

```hs
map1 :: (Int -> Bool) -> [Int] -> [Bool]
map2 :: (Double -> Char) -> [Double] -> [Char]
map3 :: (Bool -> Integer) -> [Bool] -> [Integer]
map4 :: (Integer -> Integer) -> [Integer] -> [Integer]
map5 :: (Double -> Int) -> [Double] -> [Int]
.
.
.
```

このような複数の関数を、いちどに定義したということ。
関数mapは「多相関数を引数にと」ってはいない。

### 型変数のスコープ

「多相関数を引数にとる関数」を定義する方法がある。
標準的なHaskellではムリだ。
GHCの言語拡張を使う。
それを紹介するまえに、「型変数のスコープ」を考えてみる。
もういちど、関数mapの型をみてみよう。

```hs
map :: (a -> b) -> [a] -> [b]
```

ふたつの型変数aのところには、おなじ型が、
ふたつの型変数bのところにも、やはり、おなじ型が、それぞれ、はいる。
これは型変数a, bともに、そのスコープが、関数mapの型全体であるためだ。
型変数のスコープを明示するには、予約語forallを使う。

```hs
map :: forall a b . (a -> b) -> [a] -> [b]
```

このような書きかたを有効にするには、ExplicitForAll、または、
RankNTypes拡張を指定する。

### 多相関数を引数にとる関数が定義できる

はじめに紹介した関数funを定義するには、RankNTypes拡張を指定したうえで、
型変数のスコープを明示的に限定する必要がある。
つぎのようなファイルrankNTypes.hsを作成する。

```hs:rankNTypes.hs
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

fun :: (forall a . [a] -> [a]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun f ns cs = (f ns, f cs)
```

対話環境で試してみよう。

```hs
> :load rankNTypes.hs
> fun reverse [1, 2, 3] "hello"
([3,2,1],"olleh")
```

この関数funは、関数mapとはちがい、「多相関数を引数としてと」る関数だ。

### 多相関数でなければならなくなる

たとえば、つぎのような、ふたつの定義をくらべてみる。
ファイルrankNTypes.hsに追加する。

```hs:rankNTypes.hs
foo :: ([a] -> [a]) -> [a] -> [a]
foo f xs = f xs

bar :: (forall a . [a] -> [a]) -> [b] -> [b]
bar f xs = f xs
```

対話環境で試してみる。

```hs
> :reload
> foo reverse "world"
"dlrow"
> bar reverse "world"
"dlrow"
> foo ("hello" ++) "world"
"helloworld"
> bar ("hello" ++) "world"

<interactive>:X:Y: error:
    .
    .
    .
```

関数fooでは、リストのなかみがChar型の値であることを前提とした関数を、
引数としてとることができる。
それにたいして、関数barでは、
リストのなかみの型が特定の型であるという前提を利用した関数は、
引数とすることができない。
関数barでは、リストの構造だけをいじる関数に、引数を限定できるということだ。
このあたりのことは、「ランク2多相の、ふたつの側面」でも説明した。

ランクN多相
-----------

さて、「多相関数を引数とする関数が、ランク2多相関数である」と説明した。
より正確には「ランク1多相関数を引数とする関数が、ランク2多相関数である」となる。
さらに、ランク2多相関数を引数とする関数は、ランク3多相関数であり、
ランク3多相関数を引数とする関数は、ランク4多相関数である。

つまり、「ランクN多相関数を引数とする関数が、ランク(N+1)多相関数である」となる。

まとめ
------

標準的なHaskellでは、多相関数を引数としてとる関数は定義できない。
RankNTypes拡張を指定し、予約語forallで型変数のスコープを明示することで、
多相関数を引数としてとる関数を定義することができる。
