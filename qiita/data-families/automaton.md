決定性有限オートマトンを実装する - データ族の使用例
===================================================

はじめに
--------

これは、データ族(Data Family)の簡潔な使用例だ。
型シノニム族ではなく、データ族を使うほうが自然な例として、
決定性有限オートマトン(Deterministic Finite Automaton, DFA)の
例が適していると考えた。
一番問題だったのは以下の点である。

* インスタンスの定義ごとに新しい型を定義する必然性

この例では、入力の型ごとに、「状態」を新たな型として定義する。
それぞれの入力ごとに専用の「状態」を定義するのは、自然なことと思われる。
よって、データ族の使用例として適切と考えた。

オートマトンとは
----------------

オートマトンについて、ざっくり説明すると、つぎのようになる。
入力の列が「受理」されるかどうかをチェックする、仮想的な機械。
入力の列があたえられると、内部の状態を変化させていく。
最後の入力が処理された段階で、
特定の状態になっているかどうかを調べ、
そうであれば、その入力列は受理されたということになる。

決定性有限オートマトンとは
--------------------------

有限個の状態と遷移動作の組み合わせから作られるオートマトン。
状態と入力によって、つぎの状態は、ひとつに決まる。

[Wikipedia: 決定性有限オートマトン](
	https://ja.wikipedia.org/wiki/%E6%B1%BA%E5%AE%9A%E6%80%A7%E6%9C%89%E9%99%90%E3%82%AA%E3%83%BC%E3%83%88%E3%83%9E%E3%83%88%E3%83%B3 )

入力されるものの、ひとつひとつを文字(alphabet)とよぶ。
ここで、0, 1のふたつからなる文字の列について、
0が偶数であれば受理する機械を考えよう。

![オートマトン](pngs/automaton.png)

初期状態として状態S1からはじまる。
0が入力されるたびに、状態S1とS2のあいだを行き来する。
1の入力では状態は変化しない。
最後の入力が処理されたあとに、状態S1であれば入力列は受理されたことになる。

何を作るか
----------

オートマトンの仕事は、文字(alphabet)の列を入力として受けとり、
受理かどうかを判定すること。
よって最終的に必要な関数は、つぎのようになる。

```hs
check :: [alphabet] -> Bool
```

オートマトンの種類は、ここでのalphabetの種類によって固定されることにする。
おなじ入力(たとえばBool値)に対して複数のオートマトンを定義したいときは、
newtypeを定義して、値構築子でラップして、別々の型にする。

型クラスの定義
--------------

まずは、必要な言語拡張などを書きこむ。
ファイルautomaton.hsを、つぎのように作成する。

```hs:automaton.hs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
```

型クラスを定義する。
決定性有限オートマトンは、つぎの5つの要素からなる。

* 文字集合
* 状態集合
* 開始状態
* 受理状態の集合
* 遷移関数

「文字集合」はインスタンス化される型とする。
その他の4つを含む型クラスを作ればいい。

```hs:automaton.hs
class DFA alphabet where
        data States alphabet
        initialState :: State alphabet
        accept :: State alphabet -> Bool
        transition :: State alphabet -> alphabet -> Maybe (States alphabet)
```

関数の定義
----------

オートマトンの型クラスを作ったので、つぎは受理かどうかを判定する関数を作る。
つぎのような型になるはずだ。

```hs
check :: DFA alphabet => [alphabet] -> Bool
```

関数checkを定義するために、より機械の動作に密着した関数を作る。

```hs:automaton.hs
run :: DFA alphabet =>
        States alphabet -> [alphabet] -> Maybe (States alphabet)
run s [] = Just s
run s (a : as) = (`run` as) =<< transition s a
```

関数runの定義ではMaybeモナドを使っている。
これは、遷移先が定義されていない場合に対応するためだ。
段階的に理解したいなら、つぎのような定義を考えてみよう。

```hs
run s (a : as) = run (transition s a) as
```

文字(alphabet)を消費しつつ、状態を変化させていっているのがよくわかる。
関数runを使えば関数checkを作るのは、かんたんだ。

```hs:automaton.hs
check :: DFA alphabet => [alphabet] -> Bool
check = maybe False accept . run initialState
```

関数runに引数として初期状態をあたえて、
結果がNothingのときはFalseを、そうでなければ関数acceptで判定する。

オートマトンの例
----------------

決定性有限オートマトンを表す型クラスのわくぐみを利用して、
うえでみたオートマトンの例を実装してみよう。

```hs:automaton.hs
data ExampleAlphabet = Zero | One

instance DFA ExampleAlphabet where
        data States ExampleAlphabet = S1 | S2
        initialState = S1
        accept = \case S1 -> True; S2 -> False
        transition s a = Just $ case (s, a) of
                (S1, Zero) -> S2
                (S1, One) -> S1
                (S2, Zero) -> S1
                (S2, One) -> S2
```

試してみよう。

```hs
> :load automaton.hs
> check [Zero, Zero, One]
True
> check [One, Zero, Zero, One, Zero]
False
> check [One, One, Zero, Zero, One, Zero, One, Zero]
True
```

Zeroの個数が偶数のときのみ、受理している。

まとめ
------

データ族の例として、「決定性有限状態オートマトンの入力文字列である」
という性質を表す型クラスを定義した。
入力文字列ごとに「状態」を新たに定義するのは自然なことだ。
これは、型シノニム族ではなく、データ族にする妥当な理由となるだろう。
