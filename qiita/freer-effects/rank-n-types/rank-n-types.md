Freer Effectsが、だいたいわかった: 6. 高階多相型(RankNTypes拡張)の解説
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

ランクN多相とは
--------------

### ランク1多相(rank-1 type)

標準的なHaskellのパラメトリック多相関数について考える。
たとえば、つぎのような関数がある。

```hs
maybe :: b -> (a -> b) -> Maybe a -> b
```

この型変数a, bにはどのような型でもいれることができる
(実はGHCに関しては厳密にはちがうのだが、ここでは気にしないでほしい)。

### ランク2多相(rank-2 type)

使用例
------

存在型とあわせて使う
--------------------
