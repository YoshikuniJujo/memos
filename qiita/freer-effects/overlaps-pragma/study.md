OVERLAPSプラグマなどを学ぶ
==========================

[GHC 8.2.2 User's Guide 10.8.3.6. Overlapping instances](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances )

[GHC 8.2.2 User's Guide 10.8.3.1. Instance resolution](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#instance-resolution )

ドキュメントの内容(一部)
------------------------

### インスタンス宣言の属性の決まりかた

* つぎのようなときインスタンスはincoherentである
	+ INCOHERENTプラグマがついている
	+ モジュールがIncoherentInstances拡張を使ってコンパイルされた
* つぎのようなときインスタンスはoverlappableである
	+ OVERLAPPABLEプラグマがついている
	+ OVERLAPSプラグマがついている
	+ モジュールがOverlappingInstances拡張を使ってコンパイルされた
	+ そのインスタンスがincoherentである
* つぎのようなときインスタンスはoverlappingである
	+ OVERLAPPINGプラグマがついている
	+ OVERLAPSプラグマがついている
	+ モジュールがOverlappingInstances拡張を使ってコンパイルされた
	+ そのインスタンスがincoherentである

### 適用するインスタンス宣言の選びかた

* ターゲットの型制約に合っている複数のインスタンスを候補とする。
* つぎのようなインスタンスIXを除外する
	+ IXよりも特定的なインスタンスIYが候補のなかにある
	+ IXがoverlappableであるか、またはIYがoverlappingである
* ひとつだけのincoherentではない候補が残ったら、それを選ぶ
* すべての候補がincoherentだったら、任意のひとつを選ぶ
* 上記ふたつのいずれの条件も満たさなければ、探索は失敗となる
* 選ばれた候補がincoherentであれば、探索は成功し、その候補をかえす
* そうでなければ、ターゲットの型制約とmatchはしないがunifyできる、
	インスタンスを探す。
	それらのインスタンスのすべてがincoherentであれば、選ばれた候補をかえす。
	そうでなければ探索は失件する

言語拡張
--------

Deprecated

### OverlappingInstances拡張

よりspecificなインスタンス宣言があったら、そっちを優先する。

### IncoherentInstances拡張

matchするインスタンス宣言があれば、そのうちのどれかを使用する。

プラグマ
--------

### OVERLAPPINGプラグマ

よりgenericなインスタンス宣言を上書きできる。

### OVERLAPPABLEプラグマ

よりspecificなインスタンス宣言に上書きされることができる。

### OVERLAPSプラグマ

よりgenericなインスタンス宣言を上書きでき、
よりspecificなインスタンス宣言に上書きされることができる。

### INCOHERENTプラグマ

ほかのインスタンスとの重複が許される。
重複しているインスタンスのなかで、incoherentでないものが、ただひとつあれば、
それが選ばれる。
すべてがincoherentであれば、任意のインスタンスが選ばれる。
