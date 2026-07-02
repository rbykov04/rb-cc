# rb-cc 🚀 (Compiler + IR + VM)

A toy C compiler written in Haskell, showcasing a 5-stage architecture with a flat stack-based Intermediate Representation (IR), a native x86_64 backend (AT&T syntax), and a dual-runtime execution verification engine.

---

## 🏛️ ARCHITECTURE PIPELINE

```mermaid
graph TD
    Src[Raw C Source Code] -->|Parse.hs| AST[Untyped AST]
    AST -->|Scopechecker.hs| TAST[Typed AST & Symbol Tables]
    TAST -->|StackCodegen.hs| IR[Flat Stack IR List StackOp]
    
    IR -->|Codegen.hs| X86[Native x86_64 Assembly]
    IR -->|VM.hs| HVM[Haskell State VM]
    IR -->|gen / runtime/vm.c| CVM[Bare-metal C Interpreter]
    
    X86 --> Diff[Triple Differential Test Engine]
    HVM --> Diff
    CVM --> Diff
```

---

## 📅 INTERACTIVE ROADMAP

- [x] **SPRINT 1: Frontend Cleanup** — Isolated scopechecking pass, untyped AST sanitization.
- [x] **SPRINT 2: MVP IR & Backend Infrastructure** — State-based Haskell VM, bare C runtime (`vm.c`), AT&T x86_64 generation.
- [x] **SPRINT 3: Arithmetic & Comparisons** — Binary ops (`+`, `-`, `*`, `/`, `==`, `!=`, `<`, `<=`) and recursive unary minus (`-`).
- [ ] **SPRINT 4: Variables & Control Flow** — `LoadLocal`/`StoreLocal`, flattened `Jmp`/`JmpZero` jumps, runtime visual stack frames.
- [ ] **SPRINT 5: Complex Types & Macros** — Pointers (`*`, `&`), simulated flat RAM array for VMs, string preprocessor.
---

## 🔬 END-TO-END TRANSLATION LISTING

Here is how the compiler translates a simple arithmetic expression `return -22 + 20;` across the intermediate boundaries:

| 1. C Source Code | 2. Stack-based Flat IR (`[StackOp]`) | 3. Native x86_64 Assembly (AT&T) |
| :--- | :--- | :--- |
| <pre>int main() {<br>  return -22 + 20;<br>}</pre> | <pre>PushInt 22<br>NEG<br>PushInt 20<br>ADD<br>Ret</pre> | <pre>.global main<br>main:<br>    pushq %rbp<br>    movq %rsp, %rbp<br>    pushq $22<br>    popq %rax<br>    negq %rax<br>    pushq %rax<br>    pushq $20<br>    popq %rcx<br>    popq %rax<br>    addq %rcx, %rax<br>    pushq %rax<br>    popq %rax<br>    jmp .L.return.main</pre> |

---

## 🔬 TRIPLE DIFFERENTIAL TESTING

The core strength of `rb-cc` is its **Triple Differential Testing Engine** implemented inside `VMSpec.hs`. Instead of just verifying text output, every single test case compiles the C source code down to three radically different execution targets simultaneously. 

For any C expression, `rb-cc` cross-checks results across three independent runners:

1. 💻 **Native x86_64 Execution:** The compiler generates raw AT&T assembly, assembles it via `gcc`, executes the binary as a native Linux process, and catches the exit code.
2. 🚀 **Haskell State VM:** The Intermediate Representation (`[StackOp]`) is evaluated directly inside Haskell's `State` monad emulator (`VM.hs`), simulating stack mutations in a pure environment.
3. 🛠️ **Bare-Metal C VM:** The `[StackOp]` list is serialized into a raw bytecode stream (flat array of `int`) and fed into `vm.c` — a pure, zero-dependency C byte-code interpreter loop.

```mermaid
graph TD
    Src[C Source Code] -->|compileToTypedAST| AST[Typed AST & Storage]
    AST -->|head objBody storage ! 1| Body[Raw Function Body]
    Body -->|toIR| IR[Flat IR: List StackOp]

    %% ВЕТКА 1: HASKELL EVAL
    IR -->|runVM| HVM[Haskell State VM]
    HVM -->|Result Value| Match{Do All Results Match?}

    %% ВЕТКА 2: NATIVE COMPILATION
    Src -->|./rb-cc --stack-backend | X86[variant-stack.as]
    X86 -->|gcc & run| Res3[Exit Code]
    Res3 --> Match

    %% ВЕТКА 3: ДЕТАЛИЗАЦИЯ VM2 (САМОКОМПИЛЯЦИЯ)
    subgraph Self-Bootstrap vm2
        IR -->|compile to| Bytecode[Flat Int List]
        Bytecode -->|convert to C array| DynamicC["Dynamic C main() String<br>(program[idx] = val;)"]
        StaticC[Static vm.c]
        
        StaticC & DynamicC -->|Concat: ++| Monolith["Monolithic C Source Code<br>(Engine + Injected Bytecode)"]
        Monolith -->|./rb-cc --chibicc-like-backend| Compile1["variant-chibicc.as"]
        Compile1 -->|Generates| Assembly[vm2.s]
        Assembly -->|gcc vm2.s -o vm2.exe| Executable[vm2.exe]
    end

    Executable -->|Run Binary| Res2[Exit Code]
    Src -->|./rb-cc --chibicc-like-backend| CompilePure["variant-chibicc.as"]
    CompilePure -->|gcc & run| Res1[Exit Code]

    %% Сбор всех результатов
    Res1 --> Match
    Res2 --> Match

    %% Вердикт
    Match -->|YES| Pass[🟢 GREEN SPEC]
    Match -->|NO| Fail[🔴 EXPECTATION FAILURE]



```


---

## 🗺️ ACKNOWLEDGEMENTS & INSPIRATION

`rb-cc` is heavily inspired by and follows the evolutionary step-by-step commitment approach of [chibicc]https://github.com/rui314/chibicc) by Rui Ueyama. 

While `chibicc` focuses on direct assembly generation for a registry-based machine, `rb-cc` adapts these concepts into a functional paradigm using Haskell, introducing a flat stack-based Intermediate Representation (IR) and an explicit multi-runtime verification layer (`vm.c` / `VM.hs`). Huge thanks to Rui for providing a legendary blueprint for modern toy compiler development.

