memo
====

覚え書き
--------

	StgRun(stg_returnToStackTop, &cap->r);
	rdi: stg_returnToStackTop
	rsi: &cap->r

	StgRun
	rdi -> rax
	rsi -> r13
	jmp rax

	stg_returnToStackTop
	r13: BaseReg: &cap->r
	872(%r13): CurrentTSO
	rbp: Sp: CurrentTSO->stackobj->sp

	stg_enter_info
	(%rbp): stg_enter_info
	8(%rbp): closure
	%rbp = %rbp + 16

型の説明
--------

### Capability

* OSのスレッド/タスクがHaskellのコードを走らせるのに必要なすべての情報を保持
* 以下の情報を保持する
	+ STGレジスタ
	+ TSOへのポインタ
	+ ?環境(nursery)
	+ ...
* STGを実行中はCapabilityへのポインタはBaseRegに保持される
* BaseRegは%r13(x86-64では)
* THREADED\_RTSを有効にしなければ複数のCapabilityはなく、MainCapabilityひとつになる

### TSO: Thread State Objects

* StgTSO
* たぶん、軽量スレッドを表すオブジェクトなんじゃないかな?

### Task

* OSThreadを表現する構造体
* OSThreadと1対1対応

だいたいの流れ
--------------

### C言語の関数main

&ZCMain\_main\_closureあたりを表示しておこうかな。

### hs\_main

* rts\_lock()の返り値としてMainCapabilityがかえる

### rts\_lock

* MainCapabilityのrunning\_taskをmy\_taskにする
* my\_taskのcapをMainCapabilityにする
* MainCapabilityをかえす
* などなど

### rts\_evalLazyIO

### createIOThread

* createThreadでIOThreadを作成
* pushClosureで以下のクロージャをpush
	+ stg\_ap\_v\_info
	+ closure: ZCMain\_main\_closure
	+ stg\_enter\_info: これがclosureを呼び出す感じ

### scheduleWaitThread

* taskとtsoとで互いに持ち合うなど
* capにもtaskを追加

### schedule

* たぶんt = popRunQueue(cap)でtaskを取り出す
* cap-\>r.rCurrentTSO = tでtaskをMainCapability.r.rCurrentTSOに登録
* StgRunの呼び出し
	+ %rdi: stg\_returnToStackTop
	+ %rsi: &cap-\>r

### StgRun

* %rsi -> %r13(BaseReg)
* %rdi -> %rax
* jmp \*%rax

### stg\_returnToStackTop

* LOAD\_THREAD\_STATE
* jump %ENTRY\_CODE(Sp(0))

### LOAD\_THREAD\_STATE

	tso = CurrentTSO(BaseReg->rCurrentTSO == MainCapability.r.rCurrentTSO)
	stack = tso->stackobj
	Sp = stack->sp
	SpLim = stack->stack + RESERVED_STACK_WORDS
	HpAlloc = 0

### ENTRY\_CODE

* tableNextToCodeならば、そのまま
* そうでなければCmmLoad e (bWord dflags)
	+ たぶん、...\_infoのはじめのアドレスを取り出す感じ

### stg\_returnToStackTop

つまり、

* pushClosureでpushされたstg\_enter\_infoにjump

### stg\_enter\_info

* Spは%rbp: LOAD\_THREAD\_STATEでtso-\>stackobj-\>spの値が入っている
* 引数はそれぞれ、つぎのようになっている
	+ info\_ptr: Sp[0]
	+ closure: Sp[1]
* addq $16,%rbp: これで、スタックから上記2つが消える

### ENTER

ENTER(closure)

* LOAD\_INFO(ZCMain\_main\_closure)
	+ %INFO\_PTRによって、値が読み出される
	+ それが変数infoに代入される
	+ その値はZCMain\_main\_info
* jump %ENTRY\_CODE(info) (x)
	+ 変数info(ZCMain\_main\_info)に飛ぶ
	+ (x)は引数?、よくわからない

### ZCMain\_main\_info

* R2 = c\_hello\_rq7\_closure+1
* R1 = GHC.TopHandler.runMainIO\_closure
* call stg\_ap\_p\_fast(R2, R1)
* R2への代入のときの(+1)は謎
* おそらくc\_hello\_rq7という名前のクロージャに
	runMainIOという関数を適用している

### stg\_ap\_p\_fast

* rts/dist/build/AutoApply.cmmに定義
* AutoApply.cmmはutils/genapply/Main.hsによって自動生成される
* たぶん、第1引数のクロージャに第2引数のクロージャを適用するのだと思う

### runMainIO

* Haskellの関数
* 引数のIOの例外をすべてキャッチする

### c\_hello\_rq7

* c\_hello\_rq7\_closureにc\_hello\_rq7\_infoの値がある
* c\_hello\_rq7\_infoに飛ぶのだと思う
* c\_hello\_rq7\_infoからはMain.main\_info(Main\_main\_info)を呼び出している

### Main\_main\_info

* c\_hello1\_r1d8\_infoを呼び出している

### c\_hello1\_r1d8\_info

* 中心となるあたりは、つぎのような感じ
	+ suspendThread
	+ call hello
	+ resumeThread
* スレッドを中断してからCの関数helloを呼び、またスレッドを再開する感じ

...

stg\_ap\_p\_fast以降の詳細
--------------------------

### BUILD\_PAP

### stg\_bh\_upd\_frame\_info

### stg\_marked\_upd\_frame

### updateWithIndirection

### return

* returnによって呼び出しもとではなく
	スタックトップに格納されたアドレスへ飛ぶ
* スタックトップにはstg\_ap\_vがある
	+ pushClosureで配置されていた

### stg\_ap\_v

* stg\_PAP\_applyを呼ぶ
* R1にBUILD\_PAPで作成されたpapを入れる
	+ StgPAP\_fun(pap) = GHC.TopHandler.runMainIO\_closure
	+ StgPAP\_payload(pap,0) = c\_hello\_rq7\_closure+1

### stg\_PAP\_apply

* 最終的にはGHC.TopHandler.runMainIO\_closureに飛ぶ
	+ この関数が複雑なので、より単純なものにさしかえる予定

runMainIOを単純化した場合
-------------------------

### stg\_ap\_p\_fast

* R2 = c\_hell\_rq7\_closure+1
* R1 = runMainIO\_Closure

	R1 = R1 + 1
	jump %GET_ENTRY(UNTAG(R1)) [R1, R2];

### runMainIO

* R1 = R2

	call stg_ap_0_fast(R1) args: 8, res: 0, upd: 8;

### stg\_ap\_0\_fast(fun)

* ENTER(fun)
	+ LOAD\_INFO
		- ret(x)
* return(fun)

### stg\_ap\_v

	jump %GET_ENTRY(R1-1) [R1]

### rq7\_info

* Main\_main\_info

### r1dd\_info

* call suspendThread
* call hello
* call resumeThread

GCCのレジスタの使いかた
-----------------------

* rax: 返り値
* rdi: 第1引数
* rsi: 第2引数
* rdx: 第3引数
* rcx: 第4引数
* r8 : 第5引数
* r9 : 第6引数
