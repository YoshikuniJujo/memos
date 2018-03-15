NoDatatypeContexts拡張
======================

はじめに
--------

GHCには多数の言語拡張がある。
そのうちのいくつかはデフォルトで有効になっている。
また、言語拡張の指定にHaskell98やHaskell2010を指定することで、
それぞれHaskell98やHaskell2010標準に(ほぼ)準拠したかたちでコンパイルすることが
できる。
これは、それぞれの標準に適合するように言語拡張をまとめて有効にすることで、
実現されている。

それぞれの言語標準で有効になる言語拡張
--------------------------------------

### Haskell98

* DataTypeContexts
* ImplicitPrelude
* MonomorphismRestriction
* NondecreasingIndentation
* NPlusKPatterns
* TraditionalRecordSyntax

### Haskell2010

* DataTypeContexts
* DoAndIfThenElse
* EmptyDataDecls
* ForeignFunctionInterface
* ImplicitPrelude
* MonomorphismRestriction
* PatternGuards
* RelaxedPolyRec
* TraditionalRecordSyntax

### 何も指定しないとき(GHCのデフォルト)

* DoAndIfThenElse
* EmptyDataDecls
* ForeignFunctionInterface
* ImplicitPrelude
* MonomorphismRestriction
* NonDecreasingIndentation
* PatternGuards
* RelaxedPolyRec
* TraditionalRecordSyntax

### デフォルトとHaskell2010のちがい

くらべると、デフォルトとHaskell2010とは、つぎの2点で異なる。

* デフォルトでは、DataTypeContextsがない
* デフォルトでは、NonDecreasingIndentationがある

このうちの前者、Haskell2010では有効にされるが、
デフォルトでは有効にされないDataTypeContextsについてまとめる。

この機能、あまり意味ないよね
----------------------------

Haskell98標準やHaskell2010標準では代数的データ型の定義のときに、
型クラス制約をつけることができる。
この機能について、多くは、あまり意味ない機能で、
なくしたほうがいいと考えている。
そこで、GHCのデフォルトではDataTypeContexts拡張は無効になっている。

どんな機能か、どういう役に立つ(と思われた)か
--------------------------------------------

どうして役に立たないか
----------------------

代わりの機能
------------
