# Custom Hash Map Implementations for Language Runtimes

A FreePascal Implementation with Benchmark Analysis

## 1. Problem Statement

Language interpreters and virtual machines require multiple map data structures with fundamentally different access patterns. The standard library TDictionary, while general-purpose, carries overhead from RTTI-based hashing and IEqualityComparer interface dispatch that becomes significant on hot paths such as variable resolution and property lookup.

This investigation designs, implements, and benchmarks a family of purpose-built hash maps in FreePascal (Delphi mode), each optimised for a specific runtime use case. We also compare against other RTL alternatives: TFPGMap (sorted generic map), TFPDataHashTable (chained hash table), and TStringList (sorted, binary search).

## 2. Architecture

### 2.1 Class Hierarchy

All maps inherit from a single generic base class, `TBaseMap<TKey, TValue>`, which defines the shared API contract and concrete iteration. Hash and equality logic stays in each subclass where it can be inlined or overridden as appropriate:

```text
TBaseMap<K, V>
├─ TOrderedMap<K, V>          insertion-ordered, virtual Hash/Equal
│   └─ TOrderedStringMap<V>   overrides for string content
├─ THashMap<K, V>             unordered, backshift delete, static inline
└─ TScopeMap<V>               linear scan + parent chain walk (removed — see Update)
```

### 2.2 Design Decisions

**Hash function:** DJB2 (`hash = hash * 33 + byte`). Adequate distribution, minimal code. String keys hash over characters; value-type keys over raw memory bytes.

**Ordered deletion:** Tombstone-based with compaction when tombstones exceed 50% of entries.

**Unordered deletion:** Backshift (Robin Hood). Displaced entries shift to fill gaps, keeping the table dense for GC-style bulk operations.

**Scope lookup:** Flat parallel arrays with backwards linear scan. Most recently defined bindings found first, matching lexical scoping semantics.

**Virtual vs inline:** HashKey/KeysEqual are virtual in TOrderedMap (for TOrderedStringMap override) but static inline in THashMap. GetNextEntry is virtual but only called during bulk iteration, never on the hot lookup path.

## 3. Benchmark Results

All benchmarks: FPC 3.2.2, -O2, Linux x86-64. Best of 3 iterations with loop multipliers for measurable wall-clock times.

### 3.1 String Keys: All Competitors

Five string-keyed maps compared at interpreter-relevant sizes. This is the primary comparison for object property storage.

| N (context) | Operation | TDictionary | TOrderedStringMap | TFPGMap | TFPDataHashTable | TStringList |
|---|---|---|---|---|---|---|
| 20 (scope) | Insert | 250 ns | 50 ns | 100 ns | 400,833 ns | 83 ns |
| 20 (scope) | Lookup | 67 ns | <10 ns | 50 ns | 17 ns | 50 ns |
| 100 (object) | Insert | 433 ns | 67 ns | 133 ns | 79,200 ns | 67 ns |
| 100 (object) | Lookup | 67 ns | <10 ns | 133 ns | 33 ns | 67 ns |
| 1,000 (large) | Insert | 400 ns | 467 ns | 267 ns | 8,733 ns | 200 ns |
| 1,000 (large) | Lookup | 67 ns | 67 ns | 200 ns | <10 ns | 133 ns |

TOrderedStringMap delivers 4–6× faster inserts than TDictionary at scope and object sizes (N=20–100). TFPDataHashTable has catastrophic insert performance due to chained-bucket overhead with per-node allocation. TFPGMap (sorted binary search) and TStringList are competitive on insert but slower on lookup at larger N due to O(log N) binary search vs O(1) hash. At N=1000 the hash maps converge.

### 3.2 Scope Chain: Variable Resolution

TScopeMap compared against TDictionary for scope-chain variable lookup, the hottest path in any interpreter.

| N | TDictionary lookup | TScopeMap lookup | TScopeMap resolve (3-level) |
|---|---|---|---|
| 10 | 75 ns | 50 ns | 50 ns |
| 30 | 56 ns | 111 ns | 122 ns |

TScopeMap wins at N=10 (1.5× faster) where linear scan has less overhead than hashing. At N=30 the O(n) scan crosses over and TDictionary's O(1) amortised lookup wins. The sweet spot for TScopeMap is scopes with fewer than ~20 bindings, which covers the vast majority of real-world function scopes. Its Resolve and Assign methods provide chain-walking semantics that no hash map can replicate.

> **Post-implementation finding (March 2026):** Based on these benchmarks, `TScopeMap` was implemented and deployed for scope bindings in PR #66. However, real-world profiling with macOS `sample` during integration testing revealed that the assumption — most scopes have fewer than ~20 bindings — did not hold for the global scope and bridged contexts. `CreateBridgedContext` consumed 51% of CPU samples vs 24% on `main`, a 2.7× regression. The linear-scan `IndexOf` dominated cost at the actual scope sizes encountered in practice. Scope bindings were reverted to `TOrderedStringMap<TLexicalBinding>` with recursive parent walking in `TGocciaScope`, and `ScopeMap.pas` was deleted.

### 3.3 Integer Keys (Ordered): Symbol Properties

TOrderedMap<Integer> compared against TDictionary<Integer> for symbol-keyed properties that require insertion order.

| N | Operation | TDictionary\<Integer\> | TOrderedMap\<Integer\> |
|---|---|---|---|
| 20 (typical) | Insert | 33 ns | 33 ns |
| 20 (typical) | Lookup | <10 ns | <10 ns |
| 1,000 | Insert | 38 ns | 13 ns |
| 1,000 | Lookup | 13 ns | 13 ns |

Near-parity at typical symbol counts (N=20). At N=1000, TOrderedMap is faster on insert. Both are extremely fast on integer keys. TOrderedMap provides insertion-order iteration which TDictionary cannot, making it the correct choice for spec-compliant symbol property storage regardless of performance.

### 3.4 Pointer Keys (Unordered): GC Mark Sets

| N | Operation | TDictionary\<Pointer\> | THashMap\<Pointer\> |
|---|---|---|---|
| 1,000 | Insert | 25 ns | 13 ns |
| 50,000 | Insert | 220 ns | 173 ns |
| 50,000 | Lookup | 20 ns | 27 ns |

THashMap wins on insert (the dominant GC operation) with its backshift deletion preventing tombstone accumulation across mark/sweep cycles. Lookup is comparable. The advantage grows at scale.

### 3.5 Value Type Impact: Managed vs Unmanaged

| N=100 | TOrderedStringMap\<Integer\> | TOrderedStringMap\<string\> | Overhead |
|---|---|---|---|
| Insert | 67 ns | 100 ns | +50% |
| Lookup | <10 ns | <10 ns | ~None |

String values incur ~50% overhead on insert due to reference counting and memory allocation. Lookup is unaffected since it returns by reference. For value-heavy objects where the stored value is a managed type (string, dynamic array, interface), this cost is unavoidable but should be factored into capacity planning.

## 4. RTL Alternatives Assessment

| Collection | Type | Strengths | Weaknesses |
|---|---|---|---|
| TDictionary | Generic hash map (RTTI) | Mature, well-tested, fast at scale | RTTI overhead at small N, no insertion order |
| TFPGMap | Generic sorted map | Simple, low overhead | O(log N) lookup, O(N) insert (shift) |
| TFPDataHashTable | Chained hash table | Standard RTL | Catastrophic insert: per-node heap allocation |
| TStringList (sorted) | Sorted array + binary search | Ubiquitous, simple | O(log N) lookup, string-only keys |

TFPDataHashTable should be avoided entirely in performance-sensitive code. TFPGMap and TStringList are reasonable for small, read-heavy datasets but their O(log N) lookup prevents them from competing with hash-based structures at scale.

## 5. Recommendations

| Runtime Component | Recommended Map | Rationale |
|---|---|---|
| Object string properties | TOrderedStringMap\<V\> | 4–6× faster than TDictionary at typical sizes; preserves insertion order per spec |
| Symbol-keyed properties | TOrderedMap\<TSymbolID, V\> | Parity with TDictionary; provides required insertion order |
| Scope chain variables | ~~TScopeMap\<V\>~~ → `TOrderedStringMap<TLexicalBinding>` | Originally recommended TScopeMap (linear scan, optimal at N<20). Reverted after profiling showed 2.7× regression at real-world scope sizes — see section 3.2 |
| GC mark sets, object tracking | THashMap\<Pointer, V\> | Backshift deletion prevents tombstone accumulation |
| Integer-keyed caches | TDictionary\<Integer, V\> or THashMap | FPC integer hash is trivially cheap; consider custom hash for THashMap |

## 6. Source Code (Interface Sections)

### BaseMap.pas

```pascal
{
  TBaseMap<TKey, TValue> - Abstract generic base for all map types.

  Provides:
  - Shared type aliases (TKeyValuePair, TKeyValueArray, etc.)
  - Abstract API contract (Add, TryGetValue, ContainsKey, Remove, Clear)
  - Concrete iteration (ToArray, Keys, Values, ForEach) built on
    a single abstract GetNextEntry primitive.

  Performance notes:
  - No hash or equality logic lives here. Each subclass brings its own,
    typically as static/inline class methods — zero virtual overhead on
    the inner probe loops.
  - GetNextEntry is virtual but only invoked during bulk iteration,
    never on the hot lookup path.
  - Core operations (Add, TryGetValue, etc.) are virtual for polymorphism
    but the actual work (hashing, probing, scanning) is private to each
    subclass and fully inlined.
}
unit BaseMap;
{$mode delphi}{$H+}

interface

uses
  SysUtils;

type
  TBaseMap<TKey, TValue> = class
  public type
    TKeyValuePair = record
      Key: TKey;
      Value: TValue;
    end;
    TKeyValueArray = array of TKeyValuePair;
    TKeyArray = array of TKey;
    TValueArray = array of TValue;
    TForEachCallback = procedure(const AKey: TKey; const AValue: TValue);
  protected
    function GetCount: Integer; virtual; abstract;
    function GetValue(const AKey: TKey): TValue; virtual; abstract;
    procedure SetValue(const AKey: TKey; const AValue: TValue); virtual; abstract;
    { Iteration primitive. Subclasses advance AIterState (opaque integer)
      and return the next active entry. Return False when exhausted.
      AIterState is initialized to 0 before the first call. }
    function GetNextEntry(var AIterState: Integer;
      out AKey: TKey; out AValue: TValue): Boolean; virtual; abstract;
  public
    procedure Add(const AKey: TKey; const AValue: TValue); virtual; abstract;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean; virtual; abstract;
    function ContainsKey(const AKey: TKey): Boolean; virtual; abstract;
    function Remove(const AKey: TKey): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    { Concrete iteration — built once on GetNextEntry }
    function ToArray: TKeyValueArray;
    procedure ForEach(ACallback: TForEachCallback);
    function Keys: TKeyArray;
    function Values: TValueArray;
    property Items[const AKey: TKey]: TValue read GetValue write SetValue; default;
    property Count: Integer read GetCount;
  end;
```

(Full implementation: 137 lines)

### OrderedMap.pas

```pascal
{
  TOrderedMap<TKey, TValue> - Insertion-order-preserving generic map.

  Inherits TBaseMap<TKey, TValue> for shared types and iteration.
  Hash and equality are protected virtual methods so subclasses can
  override them for specific key types (e.g. TOrderedStringMap for
  proper string hashing). Default implementation: DJB2 over raw key
  bytes + byte-level equality — correct for all fixed-size value types.

  Performance note on virtual hash/equality:
  The VMT hop per probe step is negligible in practice. For string
  keys the comparison itself dominates. For small value types the
  byte loop is branch-predictor friendly. The benefit is one ordered
  map implementation instead of two.
}
unit OrderedMap;
{$mode delphi}{$H+}

interface

uses
  SysUtils, BaseMap;

type
  TOrderedMap<TKey, TValue> = class(TBaseMap<TKey, TValue>)
  public type
    TEntry = record
      Key: TKey;
      Value: TValue;
      Hash: Cardinal;
      Active: Boolean;
    end;
    TEntryArray = array of TEntry;
  private const
    EMPTY_SLOT = -1;
    DELETED_SLOT = -2;
    INITIAL_CAPACITY = 16;
    LOAD_FACTOR_PERCENT = 70;
  private
    FEntries: TEntryArray;
    FBuckets: array of Int32;
    FCount: Integer;
    FEntryCount: Integer;
    FBucketCount: Integer;
    function FindBucket(const AKey: TKey; AHash: Cardinal;
      out ABucketIdx: Integer): Boolean;
    procedure Grow;
    procedure Rehash(ANewBucketCount: Integer);
    procedure Compact;
  protected
    { Override these for key types that need custom hash/equality (e.g. strings). }
    function HashKey(const AKey: TKey): Cardinal; virtual;
    function KeysEqual(const A, B: TKey): Boolean; virtual;
    function GetCount: Integer; override;
    function GetValue(const AKey: TKey): TValue; override;
    procedure SetValue(const AKey: TKey; const AValue: TValue); override;
    function GetNextEntry(var AIterState: Integer;
      out AKey: TKey; out AValue: TValue): Boolean; override;
  public
    constructor Create; overload;
    constructor Create(AInitialCapacity: Integer); overload;
    destructor Destroy; override;
    procedure Add(const AKey: TKey; const AValue: TValue); override;
    function TryGetValue(const AKey: TKey;
      out AValue: TValue): Boolean; override;
    function ContainsKey(const AKey: TKey): Boolean; override;
    function Remove(const AKey: TKey): Boolean; override;
    procedure Clear; override;
    function EntryAt(AIndex: Integer): TKeyValuePair;
    property Capacity: Integer read FBucketCount;
  end;
```

(Full implementation: 355 lines)

### OrderedStringMap.pas

```pascal
{
  TOrderedStringMap<TValue> - String-keyed ordered map.

  Use case: JS object string properties.
  Thin subclass of TOrderedMap<string, TValue> that overrides hash and
  equality for proper string content comparison (DJB2 on chars, native =).
  All ordered map machinery is inherited — zero duplication.
}
unit OrderedStringMap;
{$mode delphi}{$H+}

interface

uses
  SysUtils, BaseMap, OrderedMap;

type
  TOrderedStringMap<TValue> = class(TOrderedMap<string, TValue>)
  protected
    function HashKey(const AKey: string): Cardinal; override;
    function KeysEqual(const A, B: string): Boolean; override;
  end;
```

(Full implementation: 43 lines)

### ScopeMap.pas (historical — removed March 2026)

This unit was implemented based on the benchmarks in section 3.2 and deployed in PR #66. Profiling against real workloads showed that the linear-scan assumption (most scopes have <20 bindings) did not hold for global scopes and bridged interpreter contexts, causing a 2.7× regression. The unit was deleted and scope bindings reverted to `TOrderedStringMap<TLexicalBinding>`. The source is preserved here for historical reference.

```pascal
{
  TScopeMap<TValue> - Flat-array string-keyed map for scope chains.

  Use case: Variable/environment lookups in interpreter scope chains.
  Inherits TBaseMap<string, TValue> for shared types and iteration.
  No hashing — backwards linear scan, optimal for 5-30 bindings.
  Built-in parent chain walking via Resolve/Assign/Has.
}
unit ScopeMap;
{$mode delphi}{$H+}

interface

uses
  SysUtils, BaseMap;

type
  TScopeMap<TValue> = class(TBaseMap<string, TValue>)
  private
    FNames: array of string;
    FValues: array of TValue;
    FCount: Integer;
    FCapacity: Integer;
    FParent: TScopeMap<TValue>;
    procedure EnsureCapacity; inline;
    function IndexOf(const AKey: string): Integer; inline;
  protected
    function GetCount: Integer; override;
    function GetValue(const AKey: string): TValue; override;
    procedure SetValue(const AKey: string; const AValue: TValue); override;
    function GetNextEntry(var AIterState: Integer;
      out AKey: string; out AValue: TValue): Boolean; override;
  public
    constructor Create(AParent: TScopeMap<TValue> = nil);
    destructor Destroy; override;
    procedure Add(const AKey: string; const AValue: TValue); override;
    function TryGetValue(const AKey: string;
      out AValue: TValue): Boolean; override;
    function ContainsKey(const AKey: string): Boolean; override;
    function Remove(const AKey: string): Boolean; override;
    procedure Clear; override;
    function Resolve(const AKey: string; out AValue: TValue): Boolean;
    function Assign(const AKey: string; const AValue: TValue): Boolean;
    function Has(const AKey: string): Boolean;
    property Parent: TScopeMap<TValue> read FParent write FParent;
  end;
```

(Full implementation: 234 lines)

### HashMap.pas

```pascal
{
  THashMap<TKey, TValue> - Lightweight open-addressed generic hash map.

  Use case: GC tracking, caches, interning.
  Inherits TBaseMap<TKey, TValue> for shared types and iteration.
  Hash/equality are static inline — no virtual dispatch on the hot path.
  Backshift deletion — no tombstones ever.
}
unit HashMap;
{$mode delphi}{$H+}

interface

uses
  SysUtils, BaseMap;

type
  THashMap<TKey, TValue> = class(TBaseMap<TKey, TValue>)
  private type
    TSlot = record
      Key: TKey;
      Value: TValue;
      Hash: Cardinal;
      Used: Boolean;
    end;
  private const
    INITIAL_CAPACITY = 16;
    LOAD_FACTOR_PERCENT = 70;
  private
    FSlots: array of TSlot;
    FCount: Integer;
    FCapacity: Integer;
    class function HashKey(const AKey: TKey): Cardinal; static; inline;
    class function KeysEqual(const A, B: TKey): Boolean; static; inline;
    function FindSlot(const AKey: TKey; AHash: Cardinal): Integer; inline;
    procedure Grow;
    procedure Reinsert(const ASlot: TSlot);
  protected
    function GetCount: Integer; override;
    function GetValue(const AKey: TKey): TValue; override;
    procedure SetValue(const AKey: TKey; const AValue: TValue); override;
    function GetNextEntry(var AIterState: Integer;
      out AKey: TKey; out AValue: TValue): Boolean; override;
  public
    constructor Create; overload;
    constructor Create(AInitialCapacity: Integer); overload;
    destructor Destroy; override;
    procedure Add(const AKey: TKey; const AValue: TValue); override;
    function TryGetValue(const AKey: TKey;
      out AValue: TValue): Boolean; override;
    function ContainsKey(const AKey: TKey): Boolean; override;
    function Remove(const AKey: TKey): Boolean; override;
    procedure Clear; override;
    property Capacity: Integer read FCapacity;
  end;
```

(Full implementation: 266 lines)

## Update (March 2026)

### Timeline

1. **Original spike (benchmarks above).** Micro-benchmarks showed `TScopeMap` outperforming `TDictionary` at N≤10 (1.5×) and crossing over around N=30. The recommendation was to use `TScopeMap` for scope chain bindings, since most function scopes were assumed to have fewer than ~20 bindings.

2. **Implementation (PR #66).** `TScopeMap<TLexicalBinding>` was deployed for all scope bindings, with built-in parent chain walking (`Resolve`, `Assign`, `Has`). `THashMap` and `TOrderedStringMap` were also adopted for their respective use cases.

3. **Integration testing revealed regressions.** Bytecode benchmarks showed regressions of up to -42% in ArrayBuffer, TypedArray, collections, and string operations. Initial investigation focused on `THashMap` hashing costs, leading to the optimizations below (multiplicative hash, bitwise AND indexing).

4. **Profiling identified `TScopeMap` as the bottleneck.** macOS `sample` profiling showed `CreateBridgedContext` (which triggered scope chain traversal) consuming 51% of CPU samples vs 24% on `main`. The root cause: real-world global scopes and bridged interpreter contexts have significantly more than 20 bindings, pushing `TScopeMap`'s linear-scan `IndexOf` into its O(n) penalty zone. The micro-benchmark assumption (most scopes <20 bindings) was correct for function-local scopes but not for the scopes that dominate actual execution cost.

5. **Revert.** `TGocciaScopeBindingMap` was changed back to `TOrderedStringMap<TLexicalBinding>`, `TGocciaScope` reverted to recursive parent-chain walking, and `ScopeMap.pas` was deleted. This eliminated the regressions.

### Other changes since the original spike

- **`TOrderedStringMap` refactored.** It is now a standalone class inheriting `TBaseMap<string, TValue>` directly (not a thin subclass of `TOrderedMap`) with `static inline` DJB2 hash and native string equality — zero virtual dispatch on hash/equality.

- **`THashMap` hash/equality optimized.** Pointer-sized keys use multiplicative hash (golden-ratio/Fibonacci) instead of byte-by-byte DJB2, and direct integer equality instead of `CompareMem`. All `mod` operations replaced with bitwise AND.

- **Original benchmarks remain valid.** The micro-benchmarks in sections 3.1–3.5 remain valid for the general comparison at the tested sizes. The `TScopeMap` finding (section 3.2) is also correct at N=10 — the issue was that the assumption about real-world scope sizes did not hold.
