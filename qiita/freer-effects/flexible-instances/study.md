FlexibleInstances拡張を学ぶ
===========================

参考資料
--------

[GHC 8.2.2 User's Guide 10.8.3.2. Relaxed rules for the instance head](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#relaxed-rules-for-the-instance-head )

上記のドキュメントは十分ではないように思う。
以下のような例について、説明していない気がする。
確認する必要がある。

```hs
instance Foo (Bar a a) where ...
```

よく読んだら、arbitrary nested typesと書いてあるので十分だった。

FlexibleInstances拡張なしのときの制限
-------------------------------------

### Haskell 98, Haskell 2010

インスタンス宣言の頭部は以下の形である必要がある。

```hs
instance C (T a1 ... an) where ...
```

Cはクラス名、Tは型構築子(型シノニムは不可)。
nは0以上。任意のi /= jのi, jでai /= aj。

FlexibleInstances拡張で可能になること
-------------------------------------

### 型シノニムが使える

### 任意のネストした型が使える

### ただし、instance termination ruleの制限がある

[GHC 8.2.2 User's Guide 10.8.3.4. Instance termination rules](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-termination )

1. The Paterson Conditions
	1. どの型変数も、型制約(constraints on type parameters)への出現回数が
		インスタンス頭部への出現回数を越えない
	2. 型構築子と型変数の数の合計(くりかえしも数える)が、
		型制約よりもインスタンス頭部でのほうが大きい
	3. 型制約には型関数を使わない
2. 関数従属に関するルール

FlexibleInstances拡張を使うコード例
-----------------------------------

つぎのような形におさまらないような、インスタンス宣言を考える。

```hs
instance C T where ...
instance C (T a) where ...
instance C (T a b c) where ...
```

たとえば、つぎのような例。

```hs
instance C (Maybe Integer) where ...
instance C a where ...
instance C (T a a) where ...
```
