memo
====

僕が再実装したfreer-effectsにTypeFamiliesは使っていない。
それでも、ここでTypeFamiliesをあつかうのは、(すくなくとも僕にとっては)
GADTsよりもTypeFamiliesのほうが直観的に、意味がつかみやすいからだ。
Data Familiesの説明をしたうえで、
closedなData FamiliesとしてGADTsを説明するのが、
(僕にとっては)わかりやすい。

TypeFamiliesからGADTsへの流れ
-----------------------------

Type Familyというのは、こういうものですよ。

```hs
type family Foo x
type instance Foo Int = Bool
type instance Foo Bool = Char
```

型クラスといっしょに使うと便利。

関数従属で書かれたコードを書き直せますよ。
	(<- これは、微妙かなFunDepsを説明しなくてはならなくなるので...)

```hs
class ...
instance ...
instance ...
```

closedなのも書けますよ。

```hs
type family Foo x where
        Foo Int = Bool
        Foo Bool = Char
```

今のところは、このシノニム族には追加できませんよ、とだけ。
ガード的な処理によって重復した定義ができるという話は、
それを使う例が思いつかない。

同様にData Familyというのも、ありますよ。

```hs
data family Foo x
data instance Foo Int = FooInt ...
data instance Foo Bool = FooBool ...
newtype instance Foo () = FooUnit ...
```

これも型クラスと一緒に使うと便利ですよ。

```hs
class ...
instance ...
instance ...
```

ちなみに、Data Familyは、クラス関数からしか使えませんよ。
openなので値構築子が追加できてしまう。
モジュールをまたいで値構築子が存在できるので、
通常の関数だと表現できないからですよ。
拡張可能なcaseが必要になるね。

じゃあ、closedなら、どう?
残念ながら、つぎのような定義はできない。

```hs
data family Foo x where
	Foo Int = FooInt ...
	Foo Bool = FooBool ...
```

ただし、おなじことが、書きかたは違うけれどGADTsでできますよ。

```hs
data family Foo x where
	FooInt :: ... -> Foo Int
	FooBool :: ... -> Foo Bool
```

closedな型シノニム族の使い道
----------------------------

できれば、ほかの言語拡張を最小限にして、
closedな型シノニムが、ガード的な処理によって重復した定義ができるのを利用した、
実装例を作りたい。
何かないかな?

```hs
type family Foo x where
        Foo Int = Bool
        Foo a = [a]
```

のような感じで定義したものを、どのように使えばいいか。
型クラス制約とか使えるのかな。

DataKinds拡張をしないと、closedな型シノニムの良さが出ないように思う。
なので、closedな型シノニム族については、別トピックとしてあつかおうかな。

データ族の使い道
----------------

データ族でできることは、
たいてい型シノニム族でもできる(TypeFamilyDependenciesを使えば、なおさら)。
「データ族ならでは」の使用例を作りたいのだけど。
なにかないかな?
新しい型を定義したほうが自然なことって、なにかあるかな?

つまり、あるクラスを作ったときに、そのクラス専用の型が欲しくなるような状況。
なにかあるかな。
やはり、何かを保存するような状況かな。
でもリストは、型シノニム族でやったしな。
オートマトンとかはどうかな?
やってみるか。
いまいちだな。
これだと、型に対して「状態」が一意に決まるわけでもないし...
いや、「入力」の型に対する型クラスを用意してやる感じがいいかも。
やってみよう。
