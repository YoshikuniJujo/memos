書こうと思うドキュメント
========================

* 言語拡張FunctionalDependencies
* 言語拡張TypeFamilies
* 状態モナド
* Readerモナド
* Writerモナド
* Errorモナド -> Exceptモナド
* 継続モナド
* モナドトランスフォーマー
* MonadBase
* monad-control
* conduit
* .tarの作成、展開
* deflate圧縮
* PNGファイルの分析、展開、作成
* .bz2の分析、展開、圧縮
* .xzの分析、展開、圧縮
* free monad
* operational monad
* Extensible Effect
* Extensible Effectのわくぐみでconduitは可能か

何から始めるか
--------------

よく知っているものについては、モチベーションがわかない。
話が大きくなりすぎるものについては、めんどくさい。
知らないものについても、めんどくさい。
知っているけど、自分のなかで整理できていないものから、始めよう。

* MonadBase
* monad-control
* conduit

あたりを、この順にまとめてみようかな。

MonadBase
---------

まずはMonadIOの例から始めよう。
そして「MonadIOのようなもの」を一般化できるよというのを示す。
