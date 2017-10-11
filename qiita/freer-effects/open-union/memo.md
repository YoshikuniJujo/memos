memo
====

ここでは、開かれた和型について書く。
はじめに、単純な和型を示す。

```hs
data UnionValue = forall x . UnionValue x
```

比較としてつぎのような型を挙げておこう。

```hs
data Value
        = Unit ()
        | Bool Bool
        | Integer Integer
        | Double Double
        | Char Char
        deriving Show
```

そこからさらに、型引数をとる型であり、
型引数については指定できるような開かれた和型を考える。

```hs
data Union a = forall t . Union (t a)
```

これについては比較の対象として、つぎのような型を挙げる。

```hs
data SE s e a where
        Get :: SE s e s
        Put :: s -> SE s e ()
        Exc :: e -> SE s e a
```

上記のそれぞれについて、和型の値の構成と、そこからの取り出しの例を示す。
