Freer Effectsが、だいたいわかった: 3. 型族(TypeFamilies拡張)の解説
=================================================================

目次
----

0. [導入](../prelude.md)

1. [Freeモナドの概要](../free-monad/free-monad.md)
	* Freeモナドとは
	* FreeモナドでReaderモナド、Writerモナドを構成する
2. [存在型(ExistentialQuantification拡張)の解説](
	../existential-quantification/existentials.md )
3. [型シノニム族(TypeFamilies拡張)の解説](./type-synonym-family.md)
4. データ族(TypeFamilies拡張)の解説
5. 一般化代数データ型(GADTs拡張)の解説
6. FreeモナドとCoyoneda
	* Coyonedaを使ってみる
	* FreeモナドとCoyonedaを組み合わせる
		+ いろいろなモナドを構成する
7. Freerモナド(Operationalモナド)でいろいろなモナドを構成する
	* FreeモナドとCoyonedaをまとめて、Freerモナドとする
	* Readerモナド
	* Writerモナド
	* 状態モナド
	* エラーモナド
8. モナドを混ぜ合わせる(閉じた型で)
	* Freerモナドで、状態モナドとエラーモナドを混ぜ合わせる
9. 存在型による拡張可能なデータ構造(Open Union)
10. モナドを混ぜ合わせる(開いた型で)
	* FreeモナドとOpen Unionを組み合わせる
	* 状態モナドにエラーモナドを追加する
11. Open Unionを型によって安全にする
12. Freer Effectsで、IOモナドなどの、既存のモナドを使用する
13. 関数を保管しておくデータ構造による効率化
14. いろいろなEffect
	* 関数handleRelayなどを作成する
	* NonDetについて、など

データ族
--------

### データ族とは

型シノニムに対して、型シノニム族があるように、
代数的データ型の定義に対しては、データ族がある。
データ族は型シノニム族とおなじように、TypeFamilies拡張で使えるようになる。

型シノニム族とデータ族のちがいは、
型シノニムの定義と、代数的データ型の定義とのちがいとおなじだ。
それぞれのインスタンスが、
型シノニム族では既存の型に対する別名となるのに対して、
データ族では新しい型になるということだ。

それぞれの型に対して、それぞれに専用のデータ型を用意したいようなときに、
使うことができる。

### データ族を使う

### 型クラスに関連づけられたデータ族

### 閉じたデータ族?

参考
----

[Wikibooks: GHC/Type families](https://wiki.haskell.org/GHC/Type_families)
