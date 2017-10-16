Freer Effectsが、だいたいわかった: 11-1. ScopedTypeVariables拡張
================================================================

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
	1. ScopedTypeVariables拡張
	2. TypeOperators拡張
	3. KindSignatures拡張
	4. DataKinds拡張
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

型宣言をつけたい
----------------

型宣言によって、変数の型を明示することは、いい習慣だ。
型は、陳腐化することのないドキュメントだ。
また、言語拡張などを利用した複雑な型だと、
型宣言が必須になることがある。
なので、積極的に型は宣言していきたい。

型宣言がつけられない
--------------------

ふたつのリストをとり、ひとつめのリストを逆順にしたものと、
ふたつめのリストとを結合する関数を書く。

```hs:revAdd.hs
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

revAdd :: [a] -> [a] -> [a]
revAdd xs ys = rxs ++ ys
        where
        rxs = reverse xs
```

対話環境で試してみよう。

```hs
> :load revAdd.hs
> revAdd "hello" "world"
"ollehworld"
```

さて、中間値rxsに型宣言をつけてみよう。

```hs
revAdd :: [a] -> [a] -> [a]
revAdd xs ys = rxs ++ ys
        where
        rxs :: [a]
        rxs = reverse xs
```

しかし、これは型エラーになる。

暗黙のforall
------------

Haskellでは型のなかに型変数が含まれるとき、
暗黙のforallが追加される。
forallを明示してみよう。
まずは、ファイルrevAdd.hsの先頭に、つぎのような言語拡張を追加する。

```hs:revAdd.hs
{-# LANGUAGE ExplicitForAll #-}
```

そのうえで、forallを明示してみよう。

```hs:revAdd.hs
revAdd :: (forall a . [a] -> [a] -> [a])
revAdd xs ys = rxs ++ ys
        where
        rxs = reverse xs
```

対話環境で試してみる。

```hs
> :reload
> revAdd "hello" "world"
"ollehworld"
```

うえで中間値rxsに型宣言をつけようとしたが、
そのコードで、forallを明示すると、つぎのようになる。

```hs
revAdd :: (forall a . [a] -> [a] -> [a])
revAdd xs ys = rxs ++ ys
        where
        rxs :: (forall a . [a])
        rxs = reverse xs
```

それぞれの型変数aのスコープは、それぞれの型宣言のスコープのなかにある。
つまり中間値rxsの型は、revAddの型宣言における型変数aとは無関係な、
何らかの型の要素のリストであるということになる。
本来ならrxsはxsとおなじ型でなければならないため、このコードは型エラーになる。

字句的スコープをもつ型変数(lexical scoped type variables)
---------------------------------------------------------

### 明示的なforall

問題は型変数のスコープが定義される型のなかで閉じていることだ。
そのスコープを関数定義にまで、ひろげてやればいいはずだ。
そのためには言語拡張ScopedTypeVariablesが必要になる。
また、ただ型を書くと暗黙のforallによって、
型変数のスコープは型の定義のなかに制限されてしまう。

```hs
foo :: a -> b -> (a, b)
```

```hs
foo :: (forall a b . a -> b -> (a, b))
```

つまり、うえのような定義は、したのように解釈される。
そうしないためには、forallを明示する必要がある。
ファイルrevAdd.hsを、つぎのように書きなおそう。

```hs:revAdd.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

revAdd :: forall a . [a] -> [a] -> [a]
revAdd xs ys = rxs ++ ys
        rxs :: [a]
        rxs = reverse xs
```

対話環境で試しておこう。

### 暗黙のforallの抑制

ところで、rxsの型宣言にも、暗黙のforallがつくはずではないだろうか。

```hs
rxs :: [a]
```

```hs
rxs :: forall a . [a]
```

うえのような定義は、したのような定義として解釈されるはずである。
そうなれば型変数aは「新たな変数」となり、「もとのもくあみ」だ。
暗黙のforallの追加には、もうひとつルールがある。

* 型の定義のなかにある型変数が現在のスコープに、すでに存在するなら、
	暗黙のforallは追加されない

このルールがあることで、rxsの型はxsの型とおなじであることを示すことができる。

型変数を導入する、そのほかの方法
--------------------------------

### 型注釈における型変数の導入

型注釈でも型宣言とおなじように、型変数を導入することができる。

```hs:revAdd.hs
revAddAnnot = (\xs ys -> let rxs :: [a]; rxs = reverse xs in rxs ++ ys)
        :: forall a . [a] -> [a] -> [a]
```

型宣言で型変数を導入したのと、おなじように、予約語forallを明示する。
型変数のスコープを、型注釈の対象である表現へと、広げることができる。
試してみよう。

```hs
> :reload
> revAddAnnot "hello" "world"
"ollehworld"
```

### パターンでの型変数の導入

型変数をスコープに導入するやりかたは、
型宣言でforallを明示するという方法だけでなく、
「パターンに対して型注釈をつける」というやりかたもある。
つぎの例をみてみよう。

```hs:revAdd.hs
revAddPat :: [a] -> [a] -> [a]
revAddPat (xs :: [a]) ys = rxs ++ ys
        where
        rxs :: [a]
        rxs = reverse xs
```

同様に、対話環境で試しておこう。

#### 存在型について

パターンでの型変数の導入は、とくに、「存在型」を使うときに必要になる。
つぎの例をみてみよう。
ファイルexistential.hsを作成する。

```hs:existential.hs
{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data T = forall a . MkT [a]

k :: T -> T
k (MkT [t :: a]) = MkT t3
        where
        t3 :: [a]
        t3 = [t, t, t]
k _ = undefined
```

存在型を使用したデータ型Tにおいて、そのなかみの型は、
型宣言のなかに出てこない。
よって、パターンでの型変数の導入が必須になる。

型クラス宣言における型変数のスコープ
------------------------------------

型クラス宣言では、その頭部で導入された型変数のスコープは、その宣言全体となる。
これはScopedTypeVariables拡張のない場合と、おなじだ。
つぎのような、ファイルrevAddClass.hsを作成する。

```hs:revAddClass.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

class RevAdd as where
        revAdd :: as -> as -> as
```

型クラス宣言の頭部で導入された型変数aのスコープは、
型クラス宣言の全体になる。

インスタンス宣言における型変数のスコープ
----------------------------------------

インスタンス宣言では、ScopedTypeVariables拡張が有効でない場合、
宣言の頭部で導入された型変数のスコープは、頭部に限られる。
ScopedTypeVariables拡張を有効にすることで、
そのスコープをインスタンス宣言の全体に広げることができる。
ファイルrevAddClass.hsに、つぎのようなインスタンス宣言を追加しよう。

```hs:revAddClass.hs
instance RevAdd [a] where
        revAdd xs ys = rxs ++ ys
                where
                rxs :: [a]
                rxs = reverse xs
```

頭部で導入された型変数aのスコープが、インスタンス宣言の全体におよぶので、
where節でrxs :: [a]のように問題なく宣言することができる。

まとめ
------

歴史的に、Haskell(または、その前身)では、
型推論を重視し、明示的な型づけは軽視されていた印象がある。
しかし、より複雑な型が使われるようになり、
それらの有効性が明らかになるにつれ、
明示的な型づけが重視されるようになったように思う。

型宣言をつけることのできない変数があるということは、
型にドキュメントとしての機能を期待するうえで、
あるいは、複雑な型で必須になるということを考えると、
大きな問題になる。
ScopedTypeVariables拡張は、なんらかのかたちで、
Haskellの標準的な機能になることが予想される。
