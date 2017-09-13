memo
====

存在型が意味をもつ例
--------------------

1. 特定のクラスに属するという制限がある
	* かつ、なかみを取り出すための型の情報を別に保持している
2. 構成要素の引数と返り値とに、存在量化された型がある

具体例
------

1の例として、閉じた型に対応する開いた型を示す。
2の例として、...(考え中)...

### 考え中

たとえば、つぎのような型はどうだろうか。

```hs
data Cond a b = forall x . Cond (a -> x) (x -> b) (x -> b)
```

このようにしておいて、つぎのような関数を定義するとか。

```hs
cond :: Cond a b -> Bool -> a -> b
cond (Cond g f _) False = f . g
cond (Cond g _ f) True = f . g
```

どうかな?型変数xをforallにする必然性は、あるかな?
はじまりaと終わりbが、おなじ関数をリストにまとめられる、とか?

むしろ、関数リストのほうがいいかも。

data FunList a b = Id | forall x . (x -> b) :.: FunList a x

値構築子IdにGADTsが必要になるな。
やや美しさには劣るが、つぎのような定義はどうか。

data FunList a b = Fun (a -> b) | forall x . (x -> b) :.: FunList a x

こっちならExistentialQuantificationだけでできるな。
