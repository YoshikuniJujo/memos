memo
====

FreeモナドとCoyonedaとを組み合わせると、
引数をとる型をモナドにすることができる。

```hs
data Free t a
        = Pure a
        | Join (t (Free t a))

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)
```

Coyonedaをデータ型にする必要はない。

```hs
type Coyoneda t a = forall x . (t x, x -> a)
```

するとFree (Coyoneda t) aは、つぎのように展開される。

```hs
Free (Coyoneda t) a
        = Pure a
        | Join (Coyoneda t (Free (Coyoneda t) a))
```

```hs
Free (Coyoneda t) a
        = Pure a
	| forall x . Join (t x, x -> Free (Coyoneda t) a)
```

データ型なので、タプルにする必要はない。

```hs
Free (Coyoneda t) a
        = Pure a
        | forall x . Join (t x) (x -> Free (Coyoneda t) a)
```

これを、Free (Coyoneda t) aではなくFreer t aとすると、つぎのようになる。

```hs
Freer t a
        = Pure a
        | forall x . Bind (t x) (x -> Freer t a)
```

Freerモナドのデータ型について
-----------------------------

```hs
Bind (t x) (x -> Freer t a)
```

インスタンス宣言の比較
----------------------

### Functor

Freeモナドのインスタンス宣言は、つぎのようになる。

```hs
instance Functor t => Functor (Free t) where
        f `fmap` Pure x = Pure $ f x
	f `fmap` Join tx = Join $ fmap f <$> tx
```

Freerモナドでは、つぎのようになる。

```hs
instance Functor (Freer t) where
        f `fmap` Pure x = Pure $ f x
        f `fmap` Join tx k = Join tx $ k >=> Pure . f
```

### Applicative

Freeモナドでは、つぎのようになる。

```hs
instance Functor t => Applicative (Free t) where
        pure = Pure
        pure f <*> m = f <$> m
        Join tf <*> m = Join $ (<*> m) <$> tf
```

Freerモナドでは、つぎのようになる。

```hs
instance Applicative (Freer t) where
        pure = Pure
        Pure f <*> m = f <$> m
        Join tx k <*> m = Join tx $ k >=> (<$> m)
```

### Monad

Freeモナドでは、つぎのようになる。

```hs
instance Functor t => Monad (Free t) where
        Pure x >>= f = f x
        Join tx >>= f = Join $ (f =<<) <$> tx
```

Freerモナドでは、つぎのようになる。

```hs
instance Monad (Freer t) where
        Pure x >>= f = f x
        Join tx k >>= f = Join tx $ k >=> f
```
