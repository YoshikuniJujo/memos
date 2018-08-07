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

...

GCCのレジスタの使いかた
-----------------------

* rax: 返り値
* rdi: 第1引数
* rsi: 第2引数
* rdx: 第3引数
* rcx: 第4引数
* r8 : 第5引数
* r9 : 第6引数
