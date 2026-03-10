# WASM Backend

*For contributors working on the WASM emission pipeline, or host runtime implementors who need to provide the imported functions.*

The WASM backend translates Souffle bytecode modules into WebAssembly binary modules (`.wasm`). All Souffle values are represented as `externref`, with the host runtime managing the value system, constant pool, closures, and property access.

## Architecture

```text
GocciaScript Source
       │
       ▼
  Goccia.Compiler  ──►  TSouffleBytecodeModule (.sbc)
                                │
                                ▼
                    TSouffleWasmTranslator.Translate()
                                │
                                ▼
                         TWasmModule (in-memory)
                                │
                                ▼
                       .wasm binary file
                    ┌──────────┴──────────┐
                    │                     │
              souffle:constants     WASM functions
              (custom section)      (import souffle.*)
                    │                     │
                    └──────────┬──────────┘
                               ▼
                     Node.js host runtime
                    (souffle-host.mjs)
```

**Pipeline:** Source → Compiler → `TSouffleBytecodeModule` → `TSouffleWasmTranslator` → `TWasmModule` → `.wasm` binary → Node.js host

The translator sits between the existing Souffle bytecode and the WASM emitter. No changes to the compiler, VM, or bytecode format are needed.

## Usage

```bash
# Compile GocciaScript to WASM
./build/ScriptLoader example.js --emit=wasm

# Custom output path
./build/ScriptLoader example.js --emit=wasm --output=out.wasm

# Execute via Node.js host
node tests-wasm/souffle-host.mjs out.wasm

# Compile to Souffle bytecode (default)
./build/ScriptLoader example.js --emit
./build/ScriptLoader example.js --emit=bytecode
```

## Files

| File | Role |
|------|------|
| `souffle/Souffle.Wasm.Emitter.pas` | WASM binary module builder (types, imports, functions, exports, custom sections) |
| `souffle/Souffle.Wasm.Translator.pas` | Souffle bytecode → WASM translation (opcode mapping, control flow, constant pool flattening, demand-driven imports) |
| `tests-wasm/souffle-host.mjs` | Node.js host runtime that loads `.wasm` modules, extracts constants, provides `souffle` imports |
| `tests-wasm/run-wasm-tests.mjs` | Node.js test harness that compiles fixtures to `.wasm` and validates output via the Node.js host |

## Constant Pool: `souffle:constants` Custom Section

Each Souffle function template has its own local constant pool. The translator **flattens** all per-template pools into a single global pool and embeds it as a WASM custom section named `souffle:constants`. All constant index operands in the WASM code reference global indices.

### Binary Format

```text
u32          count           // number of constants
for each constant:
  u8         kind            // 0=nil, 1=true, 2=false, 3=integer, 4=float, 5=string
  [kind-dependent payload]
    kind 0: (no payload)
    kind 1: (no payload)
    kind 2: (no payload)
    kind 3: i64 (8 bytes, little-endian)
    kind 4: f64 (8 bytes, little-endian)
    kind 5: u32 byte_length + UTF-8 bytes
```

The host runtime reads this section at load time via `WebAssembly.Module.customSections()` and makes the constants available to runtime import functions (e.g., `rt_load_const`, `rt_get_prop`, `rt_get_global` all index into this pool).

## Value Mapping

All Souffle registers are translated to `externref` WASM locals. Values are boxed/unboxed through runtime import functions. The host manages the value system entirely.

| Souffle Value | WASM Representation |
|---------------|---------------------|
| `svkNil` | `externref` (host-managed null/undefined) |
| `svkBoolean` | `externref` (boxed via `rt_true`/`rt_false`) |
| `svkInteger` | `externref` (boxed via `rt_box_int`) |
| `svkFloat` | `externref` (boxed via `rt_load_const` for float constants) |
| `svkString` | `externref` (host string reference) |
| `svkReference` | `externref` (host object reference) |

## Runtime Import Contract

The WASM module imports runtime operations from a `"souffle"` module namespace. **Imports are demand-driven**: the translator scans the bytecode and only imports functions that are actually used. The host must provide all imports declared by the module.

### Value Construction

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_nil` | `(i32) → externref` | Create nil value (0=undefined, 1=null) |
| `rt_true` | `() → externref` | Create boolean true |
| `rt_false` | `() → externref` | Create boolean false |
| `rt_box_int` | `(i64) → externref` | Box an integer |
| `rt_load_const` | `(i32) → externref` | Load constant by global pool index |
| `rt_to_bool` | `(externref) → i32` | Convert to boolean (for branch conditions) |

### Arithmetic

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_add` | `(externref, externref) → externref` | Polymorphic addition (numeric + string concatenation) |
| `rt_sub` | `(externref, externref) → externref` | Subtraction |
| `rt_mul` | `(externref, externref) → externref` | Multiplication |
| `rt_div` | `(externref, externref) → externref` | Division |
| `rt_mod` | `(externref, externref) → externref` | Modulo |
| `rt_pow` | `(externref, externref) → externref` | Exponentiation |
| `rt_neg` | `(externref) → externref` | Negation |

### Bitwise

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_band` | `(externref, externref) → externref` | Bitwise AND |
| `rt_bor` | `(externref, externref) → externref` | Bitwise OR |
| `rt_bxor` | `(externref, externref) → externref` | Bitwise XOR |
| `rt_shl` | `(externref, externref) → externref` | Shift left |
| `rt_shr` | `(externref, externref) → externref` | Shift right |
| `rt_ushr` | `(externref, externref) → externref` | Unsigned shift right |
| `rt_bnot` | `(externref) → externref` | Bitwise NOT |

### Comparison

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_eq` | `(externref, externref) → externref` | Strict equality |
| `rt_neq` | `(externref, externref) → externref` | Strict inequality |
| `rt_lt` | `(externref, externref) → externref` | Less than |
| `rt_gt` | `(externref, externref) → externref` | Greater than |
| `rt_lte` | `(externref, externref) → externref` | Less than or equal |
| `rt_gte` | `(externref, externref) → externref` | Greater than or equal |
| `rt_not` | `(externref) → externref` | Logical NOT |
| `rt_typeof` | `(externref) → externref` | typeof operator |
| `rt_is_instance` | `(externref, externref) → externref` | instanceof check |

### Property Access

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_get_prop` | `(externref, i32) → externref` | Get named property (i32 = constant pool index for name) |
| `rt_set_prop` | `(externref, i32, externref) → void` | Set named property |
| `rt_get_index` | `(externref, externref) → externref` | Get indexed property |
| `rt_set_index` | `(externref, externref, externref) → void` | Set indexed property |
| `rt_del_prop` | `(externref, i32) → externref` | Delete named property |
| `rt_del_index` | `(externref, externref) → externref` | Delete indexed property |

### Invocation

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_call` | `(externref, externref, i32) → externref` | Call function (callee, args array, argc) |
| `rt_call_method` | `(externref, externref, externref, i32) → externref` | Call method (receiver, callee, args array, argc) |
| `rt_construct` | `(externref, externref, i32) → externref` | Construct with new (constructor, args array, argc) |

Arguments are packed into an `externref` array by the caller using `rt_new_array` and `rt_array_push` before invoking these functions.

### Compound Types

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_new_array` | `(i32) → externref` | Create array with capacity |
| `rt_array_push` | `(externref, externref) → void` | Push element |
| `rt_array_pop` | `(externref) → externref` | Pop element |
| `rt_array_get` | `(externref, externref) → externref` | Get by index |
| `rt_array_set` | `(externref, externref, externref) → void` | Set by index |
| `rt_new_record` | `(i32) → externref` | Create record |
| `rt_record_get` | `(externref, i32) → externref` | Get field by constant index |
| `rt_record_set` | `(externref, i32, externref) → void` | Set field |
| `rt_get_length` | `(externref) → externref` | Get length property |

### Blueprint (Class) Operations

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_new_blueprint` | `(i32) → externref` | Create blueprint (i32 = constant pool index for class name) |
| `rt_inherit` | `(externref, externref) → externref` | Set parent blueprint |
| `rt_instantiate` | `(externref) → externref` | Create instance from blueprint |
| `rt_get_slot` | `(externref, i32) → externref` | Get instance slot |
| `rt_set_slot` | `(externref, i32, externref) → void` | Set instance slot |

### Closures & Upvalues

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_closure` | `(i32) → externref` | Create closure from WASM function index |
| `rt_get_upvalue` | `(externref, i32) → externref` | Read captured variable (uses host-side `currentClosure`) |
| `rt_set_upvalue` | `(externref, i32, externref) → void` | Write captured variable (first arg = target closure) |
| `rt_close_upvalue` | `(externref, i32) → void` | Close over value |

**Closure calling convention:** The host maintains a `currentClosure` variable that tracks the currently executing closure. `rt_get_upvalue` reads from `currentClosure` (not from its first WASM argument, which is the receiver). `rt_set_upvalue` writes to its first argument (the closure being initialized). The host's `callFn` function saves/restores `currentClosure` around each closure invocation.

### Exception Handling

Exception handling uses native WASM `try`/`catch`/`throw` instructions with a shared exception tag:

| Import | Kind | Description |
|--------|------|-------------|
| `__exn_tag` | `tag (externref)` | Exception tag for `throw`/`catch` |

The translator derives try/catch structure from `OP_PUSH_HANDLER`/`OP_POP_HANDLER` pairs in the bytecode:

- `OP_PUSH_HANDLER` → opens a WASM `try` block (the opcode itself is a no-op)
- `OP_POP_HANDLER` → emits `catch $tag`, stores the caught value, branches to the catch target, then `end`
- `OP_THROW` → emits `throw $tag` with the value on the stack

The host provides a `WebAssembly.Tag({ parameters: ['externref'] })` as the `__exn_tag` import. Uncaught exceptions propagate to the host as `WebAssembly.Exception` instances. Interior forward targets (blocks whose target PCs fall inside a try range) are opened inside the try body to maintain proper WASM nesting.

### Globals & Extended Operations

| Import | Signature | Description |
|--------|-----------|-------------|
| `rt_get_global` | `(i32) → externref` | Get global by constant pool index |
| `rt_set_global` | `(externref, i32) → void` | Set global |
| `rt_has_global` | `(i32) → externref` | Check global existence |
| `rt_to_string` | `(externref) → externref` | Convert to string |
| `rt_ext_op` | `(i32, externref, externref, externref) → externref` | Extended operation (sub-opcode dispatch) |
| `rt_concat` | `(externref, externref) → externref` | String concatenation |
| `rt_to_primitive` | `(externref) → externref` | ToPrimitive coercion |

`rt_ext_op` returns its second argument (the dest value) by default. Sub-opcodes that produce new values must be implemented in the host to return the correct result.

## Control Flow Translation

Souffle uses flat jump offsets; WASM requires structured `block`/`loop`/`if`/`br`. The translator uses a **sorted targets** approach:

1. **Target collection** — Scans all jump instructions, classifying targets as forward (`block`) or backward (`loop`). For backward jumps, tracks the loop end PC (the latest back-edge source). Exception handler `CatchTarget` PCs are added as forward targets.
2. **Handler scanning** — Scans for `OP_PUSH_HANDLER`/`OP_POP_HANDLER` pairs to derive try ranges. Forward targets inside a try range are classified as "interior" and excluded from pre-opening.
3. **Block opening** — Before emission, opens `block` instructions for exterior forward targets (outermost first). Interior blocks are opened after their enclosing `try`. Loop `block`s are opened at their header PC.
4. **Emission** — Walks instructions linearly: closes try blocks at `TryEnd` (with `catch`/`br`/`end`), closes forward blocks at their target PCs, opens loops at header PCs, opens try blocks (with interior forward blocks) at `TryStart`, and converts jumps to `br`/`br_if` with computed label depths from a `LabelStack`.

Since GocciaScript has no `goto` (all control flow is structured), the jump patterns in Souffle bytecode are always reducible to structured WASM control flow.

## Function Mapping

- Each `TSouffleFunctionTemplate` becomes one WASM function
- Nested function templates are flattened into top-level WASM functions
- All functions are exported as `__fn_N` (where N is the WASM function index) for closure invocation
- The top-level function is additionally exported as `_start`
- Function indices: imports first, then defined functions
- Register 0 (receiver) is the first WASM parameter; explicit parameters follow
- One scratch `externref` local is added per function for argument array packing

## Running WASM Integration Tests

WASM output is tested via a Node.js host runtime, not through the GocciaScript test infrastructure (TestRunner/BenchmarkRunner do not support `--mode=wasm`).

```bash
# Build ScriptLoader
./build.pas loader

# Run all WASM tests
node ./tests-wasm/run-wasm-tests.mjs

# Run a specific fixture
node ./tests-wasm/run-wasm-tests.mjs tests-wasm/arithmetic.js
```

Test fixtures are GocciaScript `.js` files in `tests-wasm/` with `// Expected:` comments specifying expected stdout lines. The harness compiles each fixture to `.wasm` via ScriptLoader, executes with `node tests-wasm/souffle-host.mjs`, and compares output. Fixtures with `// Skip:` comments are skipped.

### Adding Test Fixtures

Create a `.js` file in `tests-wasm/` with `// Expected:` comments:

```javascript
// Expected: hello world
// Expected: 42
console.log("hello world");
console.log(6 * 7);
```

To skip a fixture (e.g., for unimplemented features):

```javascript
// Skip: requires async/await
// Expected: resolved
const p = Promise.resolve(42);
```

## Known Limitations

- **Host-thrown exceptions** — Runtime errors from host functions (e.g., `TypeError` from `rt_construct`) propagate as JavaScript exceptions, not WASM tag exceptions. They bypass WASM `catch $tag` blocks and propagate uncaught. Only user-level `throw` (compiled to `OP_THROW`) is caught by WASM try/catch.
- **Finally blocks** — `finally` blocks are not yet handled by the WASM translator (only `catch` is implemented)
- **Module imports** — `import`/`export` statements are not supported in WASM mode
- **Async/await** — Requires microtask queue integration, not implemented
- **Constant pool limit** — ABC-encoded instructions limit constant references to 255 per template (inherited from Souffle bytecode)
- **Endianness** — Custom section uses little-endian encoding (matching `.sbc` format)
