FlexibleContexts拡張を学ぶ
==========================

参考資料
--------

### GHC

[GHC 8.2.2 User's Guide 10.8.1.2. The superclasses of a class declaration](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-superclasses-of-a-class-declaration )

[GHC 8.2.2 User's Guide 10.8.3.3. Relaxed rules for instance contexts](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#relaxed-rules-for-instance-contexts )

[GHC 8.2.2 User's Guide 10.15.2. Relaxed rules for instance contexts](
	https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-context-of-a-type-signature )

### Haskell標準での制限

[Haskell 98標準 4.1.3 Syntax of Class Assertions and Contexts](
	https://www.haskell.org/onlinereport/decls.html )

[Haskell 2010標準 4.1.3 Syntax of Class Assertions and Contexts](
	https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-630004.1 )

### NoDataTtypeContexts

[NoDatatypeContexts - Haskell Prime](
	https://prime.haskell.org/wiki/NoDatatypeContexts )

計画
----

* Freer Effectsで必要な箇所のチェック

文法について
------------

* scontextとcontextとがある
* contextはlegacy

### 字句解析

	varid		-> (small {small | large | digit | '}<reservedid>
	coinid		-> large {small | large | digit | '}

### 基本的なところ

	modid		-> conid
	qtycls		-> [ modid . ] tycls
	tycls		-> conid
	tyvar		-> varid

### scontext

	scontext	-> simpleclass
			|  (simpleclass_1, ..., simpleclass_n) (n >= 0)
	simpleclass	-> qtycls tyvar

### context

	context		-> class
			| (class_1, ..., class_n) 	(n >= 0)
	class		-> qtycls tyvar
			| qtycls (tyvar atype_1 ... atype_n)	(n >= 1)

### contextとscontext、それぞれの使われているところ

	topdecl		-> ...
			| data [context =>] simpletype = constrs [deriving]
			| newtype [context =>] simpletype = newconstr [deriving]
			| class [scontext =>] tycls tyvar [where cdecls]
			| instance [scontext =>] qtycls inst [where idecls]
	gendecl		-> vars :: [context =>] type
			| ...
