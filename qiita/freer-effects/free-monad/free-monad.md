Freeモナドの概要
================

目次
----

モナドのおさらい
----------------

つぎのような関数が定義できるような型mをモナドと呼ぶ。

```hs
pure :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
```

おなじことを、つぎの、みっつの関数でも表現できる。

```hs
fmap :: (a -> b) -> m a -> m b
pure :: a -> m a
join :: m (m a) -> m a
```

このことは、つぎのように示すことができる。

```hs
fmap = flip (>>=) . (pure .)
join = (>>= id)

(>>=) = flip $ (join .) . fmap
```

Freeモナドを理解するうえで、モナドがfmap, pure, joinのみっつで表現される
という側面をおさえておくことが、助けとなる。
モナドとは、つぎのような操作ができる文脈mと言える。

* 値に文脈をつけることができる (pure :: a -> m a)
* 文脈のなかの値に関数を適用できる (fmap :: (a -> b) -> m a -> m b)
* 二重になった文脈を一段の文脈にほどくことができる (join :: m (m a) -> m a)

ファンクタからモナドへ
-----------------------

ファンクタとは、つぎのような関数を持つ型fのことである。

```hs
fmap :: (a -> b) -> f a -> f b
```

つまり、モナドとはファンクタであるような型fに、さらにつぎの、
ふたつの関数を追加したものである。

```hs
pure :: a -> m a
join :: m (m a) -> m a
```

モナドにできて、ファンクタにできないこと: リストの例
----------------------------------------------------

ここで、つぎのような演算子を定義する。

```hs:listExample.hs
(>>-) :: Functor f => f a -> (a -> f b) -> f (f b)
(>>-) = flip fmap
```

ファイルlistExample.hsに定義しておく。
モナドのbind関数と比較してみよう。

```hs
(>>-) :: Functof f => f a -> (a -> f b) -> f (f b)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

ファンクタのほうの演算子では、文脈が二重になっているのがわかる。
ファイルlistExample.hsを読み込んで、つぎのように試してみよう。

```hs
> [1, 2] >>= \x -> "abc" >>= \y -> [False, True] >>= \z -> [(x, y, z)]
[(1,'a',False),(1,'a',True),(1,'b',False),(1,'b',True),(1,'c',False),
(1,'c',True),(2,'a',False),(2,'a',True),(2,'b',False),(2,'b',True),
(2,'c',False),(2,'c',True)]
> [1, 2] >>- \x -> "abc" >>- \y -> [False, True] >>- \z -> [(x, y, z)]
[[[[(1,'a',False)],[(1,'a',True)]],[[(1,'b',False)],[(1,'b',True)]],
[[(1,'c',False)],[(1,'c',True)]]],[[[(2,'a',False)],[(2,'a',True)]],
[[(2,'b',False)],[(2,'b',True)]],[[(2,'c',False)],[(2,'c',True)]]]]
```

ファンクタのほうの演算では、リストがネストしてしまっている。

Freeモナドとは
--------------

もとの型がファンクタであれば、それを変換してモナドにすることができるデータ型。
つぎのような定義となる。

```hs:freeMonad.hs
data Free t a
        = Pure a
        | Join (t (Free t a))
```

再帰的な定義となっていて、たとえばリストでは、
つぎのような値を定義することができる。

```hs
Join [Join [Join [Join [Pure True]]]] :: Free [] Bool
```

Join [...]を何重に入れ子にしても、おなじ型Free [] Boolとして、
あつかうことができる。
つまり、もとの型によるデータ構造が、何重にネストしてしまったとしても、
この型の値に変換することで、ネストのない型として、
あつかうことができるということになる。

```hs
[[[[True]]]] :: [[[[Bool]]]]
Join [Join [Join [Join [Pure True]]]] :: Free [] Bool
```

値はネストしても、型はネストしない。
このように、値はネストしたままで、関数joinと同様の効果を、得ることができる。

### リストの例

リストはファンクタなので、Freeモナドによる変換が可能だ。
ここでは、対話環境で表示できるように、リスト専用のデータ型を用意して、
それを使ってみよう。
つぎのデータ型を定義する。
ファイルfreeList.hsを作成し、つぎのように定義しよう。

```hs:freeList.hs
data FreeList a
        = Pure L a
        | JoinL [FreeList a]
        deriving Show
```

整数をたし算、または、かけ算で、つぎつぎにつないでいったときの、
すべての組み合わせをもとめることを考える。
つぎのような関数を演算子(>>=)でつないでいけばいい。
ファイルfreeList.hsに、つぎの関数を追加しよう。

```hs:freeList.hs
mulAdd :: Integer -> Integer -> [Integer]
mulAdd y x = [x + y, x * y]
```

まずは、ふつうに、リストモナドとして試してみる。

```hs
> :load freeList.hs
> return 5 >>= mulAdd 3 >>= mulAdd 8 >>= mulAdd 11
[27,176,75,704,34,253,131,1320]
```

これは、つぎのような計算をしたということだ。

```hs
[
5 + 3 + 8 + 11, (5 + 3 + 8) * 11, (5 + 3) * 8 + 11, (5 + 3) * 8 * 11,
5 * 3 + 8 + 11, (5 * 3 + 8) * 11, 5 * 3 * 8 + 11, 5 * 3 * 8 * 11
]
```

おなじことを、Freeモナドで試してみる。
まずは、データ型FreeListをFunctorクラスのインスタンスにする。

```hs:freeList.hs
instance Functor FreeList where
        f `fmap` PureL x = PureL $ f x
	f `fmap` JoinL xs = JoinL $ fmap f `map` xs
```

PureLに対しては、なかの値xに関数fを適用すればいい。
JoinLに対しては、関数fmapをリストの要素すべてに、再帰的に、適用している。
おなじように、Applicativeクラスのインスタンスにする。

```hs:freeList.hs
instance Applicative FreeList where
        pure = PureL
	PureL f <*> mx = f <$> mx
	JoinL fs <*> mx = JoinL $ (<*> mx) `map` fs
```

クラス関数の定義の、うえの2行については、だいたいわかるだろう。
3行目はリストfsの要素に対して(<\*> mx)を、再帰的に、適用している。
Monadクラスのインスタンスにする。

```hs:freeList.hs
instance Monad FreeList where
	PureL x >>= f = f x
	JoinL xs >>= f = JoinL $ (f =<<) `map` xs
```

(>>=)を再帰的に適用している。
ここで、リストという構造に対しては、関数mapというファンクタ的な演算を、
使用していることに注意しよう。
それによって、リストはネストしてしまう。
そのリストのネストはJoinLによって、型としては解消されている。

リストモナドについて定義したのと、おなじ関数を型FreeListについても、
定義してみよう。

```hs:freeList.hs
mulAddF :: Integer -> Integer -> FreeList Integer
mulAddF y x = JoinL [PureL $ x + y, PureL $ x * y]
```

対話環境で試してみる。

```hs
> :reload
> return 5 >>= mulAddF 3 >>= mulAddF 8 >>= mulAddF 11
JoinL [JoinL [JoinL [PureL 27,PureL 176],JoinL [PureL 75,PureL 704]],
JoinL[JoinL [PureL 34, PureL 253],JoinL[ PureL 131,PureL 1320]]]
> :type it
it :: FreeList Integer
```

値としては、おこなった操作の手順を反映した、ネストしたリストとなっているが、
型としてはネストのない型となっている。

FreeList型からリスト型の値にするには、リストに対する演算子(=<<)である
concatMapを使えばいい。

```hs:freeList.hs
runList :: FreeList a -> [a]
runList (PureL x) = [x]
runList (JoinL xs) = runList `concatMap` xs
```

試してみよう。

```hs
> :reload
> return 5 >>= mulAddF 3 >>= mulAddF 8 >>= mulAddF 11
JoinL [JoinL [JoinL [PureL 27,PureL 176],JoinL [PureL 75,PureL 704]],
JoinL[JoinL [PureL 34, PureL 253],JoinL[ PureL 131,PureL 1320]]]
> runList it
[27,176,75,704,34,253,131,1320]
```

### IOの例

入出力の例をみてみよう。
freeIO.hsを作成し、つぎのように定義する。

```hs:freeIO.hs
data FreeIO a
        = PureIO a
	| JoinIO (IO (FreeIO a))
```

ファンクタ、アプリカティブ、モナドの定義を追加する。

```hs:freeIO.hs
instance Functor FreeIO where
        f `fmap` PureIO x = PureIO $ f x
        f `fmap` JoinIO m = JoinIO $ fmap f <$> m

instance Applicative FreeIO where
        pure = PureIO
        PureIO f <*> mx = f <$> mx
        JoinIO mf <*> mx = JoinIO $ (<*> mx) <$> mf

instance Monad FreeIO where
        PureIO x >>= f = f x
        JoinIO mx >>= f = JoinIO $ (f =<<) <$> mx
```

入出力の例として、引数の整数を表示して、
それをカウントアップしてかえす機械を考える。
まずは、通常のIOについて試す。
ファイルfreeIO.hsに、つぎの関数を追加する。

```hs:freeIO.hs
count :: Integer -> IO Integer
count n = putStrLn ("n = " ++ show n) >> return (n + 1)
```

対話環境で試してみよう。


```hs
> :load freeIO.hs
> count 8 >>= count >>= count >>= count
n = 8
n = 9
n = 10
n = 11
12
```

おなじ機械のFreeIO型バージョンを書く。

```hs:freeIO.hs
countF :: Integer -> FreeIO Integer
countF n = JoinIO $ putStrLn ("n = " ++ show n) >> return (PureIO $ n + 1)
```

FreeIO型の値は、そのままでは表示も実行もできないので、関数runIOを定義する。
ここでは、LambdaCase拡張を使っている。

```hs:freeIO.hs
runIO :: FreeIO a -> IO a
runIO = \case
        PureIO x -> return x
        JoinIO m -> m >>= runIO
```

対話環境で試してみよう。

```hs
> :reload
> action = countF 8 >>= countF >>= countF >>= countF
> runIO action
n = 8
n = 9
n = 10
n = 11
12
```

FreeIO型の値はIO型の値が入れ子になっている状態であり、
それぞれの入出力はまだ合成されていない。
それを示すために、関数runWithを定義する。

```hs:freeIO.hs
runWith :: FreeIO a -> IO b -> IO a
runWith fio act = case fio of
        PureIO x -> return x
        JoinIO m -> act >> m >>= (`runWith` act)
```

対話環境で試す。

```hs
> :reload
> action = countF 8 >>= countF >>= countF >>= countF
> action `runWith` putStrLn "hello"
hello
n = 8
hello
n = 9
hello
n = 10
hello
n = 11
12
```

FreeIO型では、それぞれの入出力がばらばらで保管されているため、
このように、あいだにほかの入出力をはさむことができる。

### 状態モナドの例

Freeモナドで、いろいろなモナドを作る
------------------------------------

### Readerモナド

### Writerモナド

### ReaderとWriterの両方

#### ReaderとWriterの意味論をそのままに

#### ReaderとWriterで状態モナドとする

```hs:hoge.hs
hello = hoge
hige = hoge
```
