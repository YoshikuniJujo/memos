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
