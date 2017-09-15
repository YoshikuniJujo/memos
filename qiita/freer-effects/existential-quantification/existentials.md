Freer Effectsが、だいたいわかった: 2. 存在型(ExistentialQuantification拡張)の解説
=================================================================================

目次
----

0. [導入](../prelude.md)

1. [Freeモナドの概要](../free-monad/free-monad.md)
	* Freeモナドとは
	* FreeモナドでReaderモナド、Writerモナドを構成する
2. 存在型(ExistentialQuantification拡張)の解説
3. FreeモナドとCoyoneda
	* Coyonedaを使ってみる
	* FreeモナドとCoyonedaを組み合わせる
		+ いろいろなモナドを構成する
4. Freerモナド(Operationalモナド)でいろいろなモナドを構成する
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
5. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
6. 存在型による拡張可能なデータ構造(Open Union)
7. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
8. Open Unionを型によって安全にする
9. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
10. 関数を保管しておくデータ構造による効率化
11. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

存在型が、なぜ存在型とよばれるか
--------------------------------

ここでは「存在型」の名前の理由を解説する。
実際に使用するときには、知らなくても問題ないので、
読みとばしてもかまわないかと思われる。

### 代数的データ型について

「代数的データ型が何をしているのか」について、考えてみよう。
つぎのような、定義があったとする。

```hs
data Foo = Foo Int
```

このとき、つぎの関数が定義されていると考えられる。

```hs
Foo :: Int -> Foo
```

しかし、それだけではない。
データ型FooからInt型の値を取り出すことを考えてみよう。

```hs
some (Foo x) = x
```

このようにパターンマッチを使う。
パターンマッチでしていることは、実は、つぎのような処理である。

```hs
unFoo :: Foo -> Int
```

つまり、代数的データ型Fooを定義するということは、
関数FooとunFooとを定義しているということだ。

```hs
data Foo = Foo Int

Foo :: Int -> Foo
unFoo :: Foo -> Int
```

### 全称量化と存在量化

演算子(.)の型を、みてみよう。

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
```

型変数a, b, cにはどのような型をいれてもいい。
つぎのような型は、演算子(.)の型として正しい。

```hs
(Int -> Bool) -> (String -> Int) -> String -> Bool
(Double -> Integer) -> (Char -> Double) -> Char -> Integer
```

つまり演算子(.)は「それぞれ、すべての型a, b, cについて」定義されている。
「すべての...について」とすることを、論理学の言葉で「全称量化」とよぶ。

	例1: 「すべてのxについて、xが10の倍数であれば、xは2の倍数である」

それに対して「ある...では...である」のようにすることを、「存在量化」とよぶ。

	例2: 「あるxについて、xは素数であり、かつ、xは偶数である」

おなじことを、より日常的な表現とすると、つぎのようになる。

	例2': 「素数であり、かつ、偶数であるようなxが存在する」

### 存在型

つぎのような代数的データ型を考える。

```hs
data Foo = Foo x
```

このような型は定義できない。
代数的データ型では、等号の左側にない型変数を、右側に置くことはできない。
GHCでは、ExistentialQuantification拡張を使い、かつ明示的に量化子forallを
つけることで、このような定義が可能となる。

```hs
data Foo = forall x . Foo x
```

つぎのような、ふたつの関数を定義したと考えることができる。

```hs
Foo :: forall x . x -> Foo
unFoo :: exists x . Foo -> x
```

実際にはexistsという量化子はHaskellには存在しない。
意味としては、つぎのようになる。

	すべての型xについてx型の値をFoo型の値に変換する関数Fooを定義
	Foo型の値を、なんらかの型xの値へ変換する関数unFooを定義

このあたりは、つぎの「箱」のイメージで理解できる。

	「何でもいれられる箱」から
	「取り出せるものは、何かしらの物」である

かりに、つぎのように、両方とも全称量化で定義されていると仮定する。

```hs
Foo :: forall x . x -> Foo
unFoo :: forall x . Foo -> x
```

すると、つぎのようになる。

	「何でもいれられる箱」からは
	「何でも取り出せる」

すべてのものを入れられる箱であっても、
その箱から、すべてのものが取り出せるわけではない。
すべてのものを入れられる箱から取り出せるのは、
「すべてのもの」のうちの「どれか」である。

Foo型の値を使った関数を考えてみよう。

```hs
useFoo :: Foo -> Int -> Bool
```

型Fooのなかに入っているのは、あらゆる型のうちの「どれか」である。
Fooという衣をはぐと、つぎのような型になる。

```hs
useFoo' :: exists x . x -> Int -> Bool
```

つまり、ExistentialQuantification拡張を使うと、
代数的データ型により、つつむことで、実質的には、存在量化された型変数を
使うことができるということになる。

存在型を使う
------------

これまでの説明では、「存在型がいったい何に使えるのか」は、わからない。
使用例をみていこう。

### もっとも単純な存在型

まずは「もっとも単純な存在型」を定義してみよう。
ファイルblackhole.hsを、つぎのように作成する。

```hs:blackhole.hs
{-# LANGUAGE ExistentialQuantification #-}

data Blackhole = forall x . Blackhole x
```

どのような型であっても、値構築子Blackholeでつつむことができる。

```hs
> :load blackhole.hs
> :type Blackhole 123
Blackhole 123 :: Blackhole
> :type Blackhole True
Blackhole True :: Blackhole
> :type Blackhole ('c', False)
Blackhole ('c', False) :: Blackhole
> :type Blackhole (Blackhole [False, True, True])
Blackhole [False, True, True] :: Blackhole
```

パターンマッチで、なかの値を取り出す関数を書いてみよう。

```hs
> some (Blackhole x) = x

<interactive>:x:y: error:
    ・ Couldn't match expected ...
         because type variable `x' would escape its scope
    ...
```

エラーとなる。
パターンマッチで値を取り出そうとしても、値xの型が決められないので、
型エラーとなる。
Blackhole型の値を使って何かすることもできないし、
Blackhole型から、なかの値を(まともな方法では)取り出すこともできない。

### 表示できるものだけに制限する

すべての型について実行できる演算には、あまり意味のあるものはない。
そこで、「すべての型」ではなく、すこし制限をつけてみよう。
まずは、「表示できる」ものだけをつつむことのできる型を作ろう。
showyou.hsを、つぎのように作成する。

```hs:showyou.hs
{-# LANGUAGE ExistentialQuantification #-}

data ShowYou = forall s . Show s => ShowYou s
```

この型ShowYouをShowクラスのインスタンスにしておこう。

```hs:showyou.hs
instance Show ShowYou where
        show (ShowYou s) = "(ShowYou " ++ show s ++ ")"
```

「Blackholeのときは取り出せなかったのに、ここでは取り出せるのは、なぜ?」
と思うかもしれない。
ここでの値sは、Showクラスのインスタンスであることが保証されている。
Showクラスのインスタンスであれば、関数showでStringに評価されるはずだ。
一度は「型のわからない値s」となったとしても、最終的にはString型になる。
そのようにして、型を決めることができる。試してみよう。

```hs
> :load showyou.hs
> ShowYou 123
(ShowYou 123)
> ShowYou (True, 'c')
(ShowYou (True,'c'))
> ShowYou (ShowYou [True, False, True])
(ShowYou (ShowYou [True,False,True]))
```

ShowYouに含まれる値は、「表示できる」という性質だけが保証されている。

### 開かれた型

存在型を使うことで、「開かれた型」を作ることができる。
より正確には「開かれた直和をもつ型」となるだろうか。

#### 閉じた型

図形を表すデータ型を考えてみよう。
ファイルclosedShape.hsを、つぎのように作成する。

```hs:closedShape.hs
data Shape
        = Rectangle (Double, Double) Double Double
	| Circle (Double, Double) Double

area :: Shape -> Double
area (Rectangle _ w h) = w * h
area (Circle _ r) = r * r * pi

areas :: [Shape] -> [Double]
areas = map area
```

試してみよう。

```hs
> area $ Rectangle (3, 8) 4 7
28.0
> area $ Circle (- 4, 5) 10
314.1592653589793
> areas [Rectangle (3, 8) 4 7, Circle (- 4, 5) 10]
[28.0,314.1592653589793]
```

データ型Shapeには値構築子RectangleとCircleとがあり、
それぞれ長方形と円とを意味する。
関数areaは、それぞれの図形の面積を計算する。

### 開いた型

うえの定義では、Shape型に含まれるのは長方形と円だけで、
新しい形を追加することができない。
ここで、ファイルopenShape.hsを、つぎのように定義しよう。

```hs:openShape.hs
data SomeShape = forall s . Shape s => SomeShape s

class Shape s where
        shapeArea :: s -> Double

area :: SomeShape -> Double
area (SomeShape s) = shapeArea s

data Rectangle = Rectangle (Double, Double) Double Double

instance Shape Rectangle where
        shapeArea (Rectangle _ w h) = w * h

data Circle = Circle (Double, Double) Double

instance Shape Circle where
	shapeArea (Circle _ r) = r * r * pi

areas :: [SomeShape] -> [Double]
areas = map area
```

試してみよう。

```hs
> :load openShape.hs
> areas [SomeShape $ Rectangle (3, 8) 4 7, SomeShape $ Circle (- 4, 5) 10]
[28.0,314.1592653589793]
```

Shapeクラスのインスタンスにすることで、
その型をSomeShape型としてあつかうことができるようになる。
これは、つぎのような型が作れるのと、おなじことだ。

```hs
data Shape
        = Rectangle ...
	| Circle ...
	| (ここに追加していくことができる)
```

### もとの値を取り出す

さて、データ型Blackholeをみたときに、
「なかの値を(まともな方法では)取り出すこともできない」と書いた。
では、「まともでない方法」では、どうか。
そう、「まともでない方法」では、取り出すことができる。

#### カリー=ハワード同型対応

ところで、「カリー=ハワード同型対応」を知っているだろうか。
かんたんに言うと、「ある型に対する、まともな関数が定義できる」ということが、
「ある命題を証明できる」ということに対応するということだ。

たとえば、(a -> a)型の関数としてidが定義できる。
(a -> a)型に対応する命題は、「aならばaである」なので、たしかにこれは正しい。
あるいは(a -> b, b -> c) -> (a -> c)型の関数として、uncurry $ flip (.)が定義できる。
この型に対応する命題は「(aならばb、かつ、bならばc)ならば(aならばc)である」だ。
これも、やはり正しい。

#### 関数unsafeCoerce

関数unsafeCoerceというものがある。
この関数は数々のunsafe...のなかでも、断トツでunsafeだ。
試してみよう。

```hs
> :module Unsafe.Coerce
> unsafeCoerce True :: Int
-576460205185149809
> unsafeCoerce 123 :: (Int, Char)
(zsh: segmentation fault stack ghci
```

なんということでしょう。
Haskellを始めて以来、ついぞ見ることのなかったsegmentation fault、
なつかしのあの人に再会できました。
型を見てみましょう。

```hs
> :module Unsafe.Coerce
> :type unsafeCoerce
unsafeCoerce :: a -> b
```

これをカリー=ハワード同型対応で考えると、
「任意の前提aから、任意の帰結bが導ける」ことになってしまう。
つまり、unsafeCoerceは「まとも」じゃない。

#### Blackholeから値を取り出す

さてunsafeCoerceを使うと、Blackhole型の値から、なかの値を取り出すことができる。

```hs
> :load blackbox.hs
> :module + Unsafe.Coerce
> reborn (Blackhole x) = unsafeCoerce x
> reborn $ Blackhole True :: Bool
True
> reborn $ Blackhole 'c' :: Char
'c'
> reborn $ Blackhole False :: Integer
139892841015216
```

もちろん、入れた値の型と異なる型を指定すれば、
わけのわからない結果となる。

### もとの型を安全に取り出す

より安全に値を取り出すには、型の情報を保存しておく必要がある。
つぎのようなファイルwithType.hsを作成する。

```hs
{-# LANGUAGE ExistentialQuantification #-}

import Unsafe.Coerce

data WithType = forall x . WithType String x

fromInt :: Int -> WithType
fromInt = WithType "Int"

fromBool :: Bool -> WithType
fromBool = WithType "Bool"

fromChar :: Char -> WithType
fromChar = WithType "Char"

toInt :: WithType -> Maybe Int
toInt (WithType "Int" x) = Just $ unsafeCoerce x
toInt _ = Nothing

toBool :: WithType -> Maybe Bool
toBool (WithType "Bool" x) = Just $ unsafeCoerce x
toBool _ = Nothing

toChar :: WithType -> Maybe Char
toChar (WithType "Char" x) = Just $ unsafeCoerce x
toChar _ = Nothing
```

試してみる。

```hs
> :load withType.hs
> i = fromInt 123
> b = fromBool True
> c = fromChar 'c'
> toInt i
Just 123
> toBool b
Just True
> toInt c
Nothing
```

安全に取り出せているのがわかる。
さらに、つぎのように試してみよう。

```hs
> x = WithType "Char" True
> toChar x
Just '\-576460202777211265'
```

大変なことになっている。
関数from...だけを使い、値構築子WithTypeを直接、使わないようにする必要がある。
モジュールの機能を利用して、値構築子WithTypeを隠蔽する必要がある。

### 関数のリスト

存在型の使用例をもうひとつ、みてみよう。
たとえば、複数の関数を合成する代わりに、リストに保存しておきたいとする。
つぎのような関数の合成を考える。

```hs
reverse . show . (* 3) . (+ 5)
```

このように関数を合成する代わりにリストに保存しておきたい。
しかし、つぎのようには、できない

```hs
[reverse, show, (* 3), (+ 5)]
```

「それぞれの関数の型が、すべておなじ」ではないからだ。
型が、(c -> d), (b -> c), (a -> b), ...のようになっている要素を、
リストとして保存できるデータ型を作る。
ファイルfunList.hsを作成する。

```hs:funList.hs
{-# LANGUAGE ExistentialQuantification #-}

infixr 9 :.:
data FunList a b = Fun (a -> b) | forall x . (x -> b) :.: FunList a x

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :.: fs) = f . apply fs
```

試してみよう。

```hs
> (reverse :.: show :.: (* 3) :.: Fun (+ 5)) `apply` 2
"12"
```

### 途中経過を表示する

うえの型FunList a bは、効率などを考えなければ、型(a -> b)と、
ほとんど、おなじものだ。
関数applyによって、値に適用してやるぐらいしか、できることはない。
もうすこし意味のあることをさせてみよう。
ファイルshowProgress.hsを、つぎのように作成する。

```hs:showProgress.hs
{-# LANGUAGE ExistentialQuantification #-}

infixr 9 :.:
data FunList a b
        = Fun (a -> b)
        | forall x . Show x => (x -> b) :.: FunList a x

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :.: fs) = f . apply fs

showProgress :: Show a => FunList a b -> a -> (b, [String])
showProgress (Fun f) x = (f x, show x)
showProgress (f :.: fs) x = (f y, show y : ps)
	where (y, ps) = showProgress fs x
```

試してみよう。

```hs
> :load showProgress.hs
> (reverse :.: show :.: (* 3) :.: Fun (+ 5)) `apply` 2
"12"
> (reverse :.: show :.: (* 3) :.: Fun (+ 5)) `showProgress` 2
("12",["\"21\"","21","7","2"])
```

途中経過を文字列のリストとして、取り出すことができた。

まとめ
------

標準的なHaskellでは、data Foo = Foo xのように、
等号の左側にない型変数を右側で使うような定義はできない。
ExistentialQuantification拡張を設定したうえで、
明示的に量化子forallをつけることで、このような定義が可能となる。
このように定義した型を、関数に使うことは存在量化された型を使うのと、
おなじ意味になる。

使用例としては、値構築子を追加できるデータ型と同等のものを作ることや、
関数合成する代わりに関数を保管するリストを作ることを挙げた。
これらは、いろいろな型を、ひとつの型にまとめるような、かたちになっている。

参考
----

[Wikibooks: Haskell/存在量化された型](https://ja.wikibooks.org/wiki/Haskell/%E5%AD%98%E5%9C%A8%E9%87%8F%E5%8C%96%E3%81%95%E3%82%8C%E3%81%9F%E5%9E%8B)
