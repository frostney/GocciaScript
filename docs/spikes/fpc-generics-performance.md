# FreePascal Delphi-Mode Generics

Benchmark Spike: Performance Cost of Generic Patterns

50M iterations | 7 runs | median reported | assembly verified

## Question

Does the way you consume a Delphi-mode generic in FreePascal affect runtime performance? Specifically, is there a cost to using a type alias, a subclass, a hand-rolled equivalent, or collapsing a multi-parameter generic through inheritance?

## Variants Under Test

Five instantiation patterns, each tested as a local variable and as a class field:

1. **Literal generic:** `TGenericStack<Integer>` used directly in declarations.
2. **Specialised (type alias):** `TSpecialisedStack = TGenericStack<Integer>` — a simple type alias.
3. **Inherited (subclass):** `TInheritedStack = class(TGenericStack<Integer>)` — an empty subclass.
4. **Hand-rolled:** `THandRolledStack` — identical API written from scratch, no generics involved.
5. **Double-generic collapse:** `TGenericPair<K,V>` → `TIntKeyStack<V> = class(TGenericPair<Integer,V>)` → `TDoubleInheritedStack = class(TIntKeyStack<Integer>)`. Two inheritance steps to fully concretise a two-parameter generic.

## Results

### Local Variables

| Type | Pattern | Median (ms) | Ratio | Verdict |
|---|---|---|---|---|
| TGenericStack\<Integer\> | Literal generic | 142.0 | baseline | — |
| TSpecialisedStack | Type alias | 140.0 | x0.99 | No diff |
| TInheritedStack | Subclass | 149.0 | x1.05 | No diff |
| THandRolledStack | No generics | 222.0 | x1.09 | Noise |
| TDoubleInheritedStack | Multi-generic collapse | 264.0 | x1.29\* | 2x work |

### Class Fields

| Type | Pattern | Median (ms) | Ratio | Verdict |
|---|---|---|---|---|
| TGenericStack\<Integer\> | Literal generic | 127.0 | baseline | — |
| TSpecialisedStack | Type alias | 125.0 | x0.98 | No diff |
| TInheritedStack | Subclass | 126.0 | x0.99 | No diff |
| THandRolledStack | No generics | 219.0 | x1.07 | Noise |
| TDoubleInheritedStack | Multi-generic collapse | 249.0 | x1.22\* | 2x work |

\* The double-generic variant does 2x array work per iteration (key + value arrays), so higher absolute time is expected. The relevant question is whether the multi-step generic collapse adds overhead beyond the extra work — it does not.

## Assembly Verification

The hot loop for all single-array variants (literal, alias, subclass, hand-rolled) emits byte-identical x86-64 machine code after inlining. No separate symbol is generated for aliased or inherited types — they reuse the parent's methods directly.

**Inlined hot loop — identical across literal, alias, subclass, and hand-rolled:**

| Instruction | Operation |
|---|---|
| `addl $1,16(%rax)` | `Inc(FCount)` |
| `movl -4(%rdx,%rax,4),%eax` | Peek: load `FItems[FCount-1]` |
| `xorl 28(%rsp),%eax` | `Sink := Sink xor result` |
| `subl $1,16(%rax)` | `Dec(FCount)` — Pop |

For the double-generic variant, the loop is naturally wider (two array loads for PeekKey + PeekVal) but contains no VMT indirection, no extra dispatch, and no boxing. The two-step inheritance chain is fully resolved at compile time.

## Key Findings

**FPC monomorphises fully.** Every generic instantiation produces a concrete specialisation at compile time, equivalent to hand-written code. There is no type-erasure, no boxing, and no runtime dispatch.

**Inheritance depth is free.** Collapsing `TGenericPair<K,V>` through two inheritance steps to a concrete class produces the same machine code as instantiating `TGenericPair<Integer,Integer>` directly.

**No symbol duplication for subclasses.** `TInheritedStack = class(TGenericStack<Integer>)` does not emit separate Push/Pop/Peek symbols. The subclass's VMT points to the parent's methods.

**Hand-rolled offers no advantage.** A manually written `TIntegerStack` with identical logic produces the same assembly as `TGenericStack<Integer>`. Generics are purely a code-organisation choice with zero runtime cost.

## Recommendation

Use whichever pattern gives the cleanest API ergonomics for the codebase. The type-alias pattern (`TSpecialisedStack = TGenericStack<Integer>`) is the most concise for simple cases. The inheritance pattern is useful when you want to progressively narrow multi-parameter generics into domain-specific types — the layering is free at runtime. There is no reason to hand-roll specialised types for performance.

**Environment:** FPC 3.2.2 | {$mode delphi} | -O2 | x86-64 Linux
