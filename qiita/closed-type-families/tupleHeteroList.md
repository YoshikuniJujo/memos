タプルで作ったリストの要素を型で取り出す - 閉じた型シノニム族の使用例
=====================================================================

対象読者
--------

* Haskellの初歩的な内容は理解した
* 以下の言語拡張のどちらかを使ったことがある、または、
		どちらかが使われたコードを読んだことがある
	+ FunctionalDependencies
	+ TypeFamilies
* 型演算という言葉が出てきても、動揺しない

はじめに
--------

型シノニム族(type synonym family)は、GHCのTypeFamilies拡張で使えるようになる
機能である。
型シノニムは型引数を取ることができるが、
標準では型引数は型変数のみだ。
たとえば、つぎのようになる。

```hs
type Foo x = Either String [Maybe x]
```

これは、型引数の指定により、型シノニムによって示される型の構造そのものは、
影響を受けないということだ。
型シノニム族の機能を使うと、つぎのような定義が可能になる。

```hs
type family Foo x

type instance Foo Integer = Maybe Integer
type instance Foo Char = Either Int Char
type instance Foo Bool = Double
```

型引数に具体的な型をとることができる。
結果として、型シノニムによって示される型の構造は、ばらばらになる。
つまり、型シノニム族の機能によって作られる型シノニムを使う場合、
たいていにおいて、パラメトリック多相関数ではなく、
型クラスを介したアドホック多相関数を使うことになる。

型シノニム族は、しばしば関数従属(FunctionalDependencies拡張)の
代替として使われるが、より広い用途を持ち、
型演算において中心的な役割を持っていると考えられる。

型シノニム族の定義には、新しい引数についての定義を、
自由に追加できる「開かれた型シノニム族(open type synonym family)」があり、
モジュールをまたいだ定義も可能となる。
一方、より制限された「閉じた型シノニム族(closed type synonym family)」
という定義のしかたもあり、
こちらでは、ひとつの型シノニム族に属する型シノニムは、
一ヶ所にまとめて定義しなくてはならない。

「閉じた型シノニム族」では、そのような制限によって、
逆に、できることが増えたという側面もある。
一ヶ所で定義できるということで、
型シノニムの定義を「うえから順に」試していくということが可能となる。
これは、つぎのような定義ができることを意味する。

```hs
type family Equal x y where
	Equal a a = 'True
	Equal a b = 'False
```

後者のパターンは、前者のパターンを含む。
よって、このような定義では、型シノニムの定義に「順番」をつけにくい
「開かれた型シノニム族」においては、
どちらの定義を選ぶかということの解決が難しくなる。

このような、定義の試される順番に依存した定義ができるのが、
「閉じた型システム」の大きな長所である。
この長所を生かしたサンプルコードを、ここで紹介する。

何を作るか
----------

タプルをヘテロリストとしてあつかい、
その要素を「型」によって取り出す。
ここで、ややこじつけではあるが、つぎの条件をつける。

* タプルのなかに、おなじ型の値が出てこない

まずは、この条件を強制するのではなく、
推奨されている「この関数」を使ってもらうという、
ゆるいやりかたとする。
つぎに、「新しい型を作る」ことで、
この条件を「強制」するバージョンを作る。

追加の言語拡張とプラグマ
------------------------

### DataKinds拡張

この拡張を使うと、値として定義された表現を「型」として使うことができる。
値を定義すると、型の世界にもおなじ形のものが生成される、といった感じだ。
たとえば、この拡張を使うと、値の世界でではなく、
型の世界にFalse, Trueといった名前のものが定義される。
型Falseや型Trueの種類(kind)はBoolとなる。

#### しるしをつける(tick)

Haskellでは、型の名前と値構築子の名前とは名前空間が異なるので、
それら別々のものに、おなじ名前を使うことができる。
たとえば、[]はリスト型の名前と、
空リストを表す値構築子の名前の両方に使われている。
そのため、つぎのような定義では、DataKinds拡張によって、
型へと昇格した空リストなのかが、はっきりしない。

```hs
foo :: Foo [] Int
```

このようなとき、型へと昇格させたという、しるしをつけることができる。

```hs
foo :: Foo '[] Int
```

混乱しないためには、そのような重復がない場合にも、このような、しるしを
つけておくほうが無難だ。
しるしをつけていない昇格させた値構築子に対して、警告してほしければ、
GHCにつぎのどちらかのオプションを設定する。

```hs
-fwarn-unticked-promoted-constructors
-Wunticked-promoted-constructors
```

### MultiParamTypeClasses拡張

この拡張を使うと、型クラスの型引数が複数取れるようになる。

```hs
class Foo a b where
	foo :: a -> b -> ...
```

型引数がひとつの型クラスが「型のもつ性質」だとすると、
型引数を複数もつ型クラスは「型と型との関係」と考えることができる。

### FlexibleInstances拡張

標準的な文法では、
インスタンス宣言において、インスタンスになる型は、
具体的な型構築子に、
(もし型引数をとるならば)型引数として、型変数のみをとるものでなければならない。
また、それらの型変数に「おなじ変数」があってはならない。
この制限を緩和するのがFlexibleInstances拡張だ。

### TypeOperators拡張

Haskellには、関数を演算子として書く記法がある。
また、値構築子も演算子として記述することができる。
これを拡張して、型構築子も演算子として書くことを可能にするのが、
TypeOperators拡張だ。

### OVERLAPPABLEプラグマ

インスタンス宣言をするとき、
より一般的な型に対するインスタンス宣言と、
より特殊な型に対するインスタンス宣言の両方を定義したいことがある。
そのようなときに、より一般的な型へのインスタンス宣言のほうに、
{-# OVERLAPPABLE #-}のように、プラグマをつけておいてやると、
より特殊な型のほうが優先的に使用される。
たとえば、つぎのような定義ができる。

```hs
data Foo = Foo

class Bar b where
	bar :: b -> String

instance Bar Foo where
	bar _ = "foo"

instance {-# OVERLAPPABLE #-} Bar t where
	bar _ = "bar"
```

このようにして定義しておくことで、つぎのような評価ができる。

```hs
> bar Foo
"foo"
> bar True
"bar"
```

ちなみに、類似のプラグマにOVERLAPPING, OVERLAPS, INCOHERENTがあるが、
この余白は、これらを説明するには狭すぎる(たぶん、そのうち書くと思う)。

タプルで作ったリストの要素を型で取り出す
----------------------------------------

まずは、言語拡張を定義しておく。

```hs:tuple.hs
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
```

-Wallは、unticked-promoted-contructorsを含め、すべての警告を有効にする。
-fno-warn-tabsはタブの使用に文句を言わせないために指定する。

何らかの型vsの値から、何らかの型vの値を取り出せるという型クラスを定義する。

```hs:tuple.hs
class Get v vs where
        get :: vs -> v
```

タプルとその要素とを、この型のインスタンスにする。
まず、タプル(で作ったリスト)の先頭が、
取り出そうとしている型の値だったときの定義をする。

```hs:tuple.hs
instance Get v (v, vs) where
        get (x, _) = x
```

つぎに、取り出そうとしている型とタプル(で作ったリスト)の先頭の値の型とが、
異なっているときの定義をする。

```hs:tuple.hs
instance {-# OVERLAPPABLE #-} Get v vs => Get v (_w, vs) where
        get (_, xs) = get xs
```

このパターンにはvと\_wとは、異なる型であって、おなじ型であってもマッチする。
しかし、OVERLAPPABLEプラグマによって、「より特殊なパターン」のほうが
優先するというルールが指定されているため、さきに、うえのほうのパターンとの
マッチが試される。

これで、型によって値を取り出すことができる。

```hs
> :load tuple.hs
> get ('c', (True, ("hello", ()))) :: Bool
True
```

ここで、作成されるタプル(で作ったリスト)に、
おなじ型の値が含まれないようにしたい。
タプルを直接、定義するのではなく、
新しくスマート値構築子を定義して、そちらを使うようにする。

まずは、タプルに特定の型が含まれているかどうかを確認するための
型を定義する。

```hs
type family Elem t ts where
        Elem _ () = 'False
	Elem t (t, _) = 'True
	Elem t (_, ts) = Elem t ts
```

ここで、この記事の本題である「閉じた型シノニム族」の
「型の定義をうえから順に試していける」という性質を利用している。

これを利用して、スマート値構築子を定義する。

```hs
infixr 5 .:

(.:) :: Elem t ts ~ 'False => t -> ts -> (t, ts)
(.:) = (,)
```

この値構築子を試してみよう。

```hs
> :reload
> 'c' .: True .: "hello" .: ()
('c',(True,("hello",())))
> False .: it
<interactive>:X:Y: error:
    ・Couldn't match type `'True' with `'False'
        arising from a use of `.:'
    ・In the expression: False .: it
      In an equation for `it': it = False .: it
```

このスマート値構築子(.:)だけを使えば、
タプル(で作ったリスト)に
「取り出せない値」ができてしまうことを、避けることができる。

型の重複を許さないために新しい型を定義する
------------------------------------------

さて、本題はここまでだが、やはり「(,)を使わないでね」という
紳士協定にたよった「安全性の保証」というものは、すこし気持ちが悪い。
新しい型とモジュールシステムで、「重復する型の値を含まない」という
制約を強制してみよう。
本質的には、うえの定義とおなじである。
つぎのようなモジュールを作る。

```hs:HeteroList.hs
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList (empty, (.:), get) where

data a :.: b = a :.: b deriving Show

class Get v vs where
        get :: vs -> v

instance Get v (v :.: vs) where
        get (x :.: _) = x
instance {-# OVERLAPPABLE #-} Get v vs => Get v (_w :.: vs) where
        get (_ :.: xs) = get xs

type family Elem t ts where
        Elem _ () = 'False
        Elem t (t :.: _) = 'True
        Elem t (_ :.: ts) = Elem t ts

infixr 5 .:

empty :: ()
empty = ()

(.:) = Elem t ts ~ 'False => t -> ts -> (t :.: ts)
(.:) = (:.:)
```

試してみる。

```hs
> :load HeteroList.hs
> sample = 'c' .: True .: "hello" .: ()
> get sample :: Bool
True
> False .: sample

<interactive>:X:Y: error:
    ・Couldn't match type `'True' with `'False'
        arising from a use of `.:'
    ・In the expression: False .: sample
      In an equation for `it': it = False .: sample
```

ここで作られた構造には、おなじ型の値が含まれていないことが、保証される。

まとめ
------

「閉じた型シノニム族」には、型の定義を「うえから順に試す」という性質がある。
この性質を利用して型Elemを作成した。
この型Elemの使用例として、「型によって値を取り出せるヘテロリスト」の例を挙げた。
