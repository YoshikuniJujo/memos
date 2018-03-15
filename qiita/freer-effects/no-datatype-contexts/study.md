NoDatatypeContextsを学ぶ
========================

参考ページ
----------

[言語標準と言語拡張について](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/language-standards)

[ghc/compiles/main/DynFlags.hs:2090](https://github.com/ghc/ghc/blob/master/compiler/main/DynFlags.hs#L2090)

```hs:ghc/compiler/main/DynFlags.hs#L2090-2124
languageExtensions :: Maybe Language -> [LangExt.Extension]

languageExtensions Nothing
    -- Nothing => the default case
    = LangExt.NondecreasingIndentation -- This has been on by default for some time
    : delete LangExt.DatatypeContexts  -- The Haskell' committee decided to
                                       -- remove datatype contexts from the
                                       -- language:
   -- http://www.haskell.org/pipermail/haskell-prime/2011-January/003335.html
      (languageExtensions (Just Haskell2010))

   -- NB: MonoPatBinds is no longer the default

languageExtensions (Just Haskell98)
    = [LangExt.ImplicitPrelude,
       LangExt.MonomorphismRestriction,
       LangExt.NPlusKPatterns,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.NondecreasingIndentation
           -- strictly speaking non-standard, but we always had this
           -- on implicitly before the option was added in 7.1, and
           -- turning it off breaks code, so we're keeping it on for
           -- backwards compatibility.  Cabal uses -XHaskell98 by
           -- default unless you specify another language.
      ]

languageExtensions (Just Haskell2010)
    = [LangExt.ImplicitPrelude,
       LangExt.MonomorphismRestriction,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.EmptyDataDecls,
       LangExt.ForeignFunctionInterface,
       LangExt.PatternGuards,
       LangExt.DoAndIfThenElse,
       LangExt.RelaxedPolyRec]
```

標準における言語拡張
--------------------

### Haskell98

* ImplicitPrelude
* MonomorphismRestriction
* NPlusKPatterns
* DataTypeContexts
* TraditionalRecordSyntax
* NondecreasingIndentation

### Haskell2010

* ImplicitPrelude
* MonomorphismRestriction
* DatatypeContexts
* TraditionalRecordSyntax
* EmptyDataDecls
* ForeignFunctionInterface
* PatternGuards
* DoAndIfThenElse
* RelaxedPolyRec

### デフォルト

* NondecreasingIndentation
* ImplicitPrelude
* MonomorphismRestriction
* TraditionalRecordSyntax
* EmptyDataDecls
* ForeignFunctionInterface
* PatternGuards
* DoAndIfThenElse
* RelaxedPolyRec
