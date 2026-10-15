# Issue #1704 Investigation: Source Literal Memory Leak

## Problem Statement
A 100KB string literal in source code (`const s = "xxx...";`) causes ~607 KB/iteration memory growth after engine free + GC collect, while runtime-generated strings (`"x".repeat(100000)`) stay flat.

## Reproduction
```bash
./build/GocciaMemHost seqgc 100 repro_literal.js
# Result: 21480 kB → 82156 kB (60.7 MB growth)

./build/GocciaMemHost seqgc 100 repro_runtime.js  
# Result: 14540 kB → 14668 kB (flat, ~128 KB overhead)
```

## Investigation Results

### Ruled Out
1. **Diagnostic Source Registry**: Separate test confirmed scope properly frees memory (stays flat at ~4 MB after 100 iterations of 100KB source registration + free)

2. **Pascal Copy() Behavior**: Test confirmed Copy() creates new allocations that ARE properly freed when container is freed (21 MB → 1.4 MB after freeing array)

3. **Explicit Constant Pool Cleanup**: Adding manual `StringValue := ''` and `SetLength(FConstants, 0)` in template destructor had no effect

### Key Code Paths Identified

**String Allocation Points:**
1. **Lexer** (`Goccia.Lexer.pas:1078-1107`): Builds string character-by-character into `TStringBuffer`, then `SB.ToString` creates new allocation stored in `Token.Lexeme`

2. **Parser** (`Goccia.Parser.pas:2806`): Calls `ExtractSourceRange` which does:
   ```pascal
   SourceText := Previous.Span.Source;  // Gets FULL 100KB+ source
   Result := Copy(SourceText, StartOffset + 1, EndOffset - StartOffset);
   ```
   Creates another copy for AST node construction

3. **Constant Pool** (`Goccia.Bytecode.Chunk.pas:854-869`):
   ```pascal
   FConstants[FConstantCount].StringValue := AValue;  // Reference assignment
   FStringConstantIndex.Add(AValue, Result);  // Map stores key copy
   ```

**Destruction Chain** (Verified correct order):
```
Runtime.Destroy (line 646)
└─> Engine.Free
    ├─> FRetainedModules.Free (line 1375) - TObjectList with OwnsObjects=True
    │   └─> TGocciaBytecodeModule.Free
    │       └─> TGocciaFunctionTemplate.Free
    │           └─> FConstants finalized (strings should be released)
    │           └─> FStringConstantIndex.Free
    └─> FModuleLoader.Free (line 1406)
        └─> FDiagnosticScope.Free (line 713)
```

### Measurements
- Literal size: 100 KB
- Growth per iteration: ~607 KB (6x literal size)
- This suggests multiple copies or overhead structures

### Hypotheses Not Yet Proven
1. **FStringConstantIndex retention**: Map keys are string copies, but should be freed with map
2. **GC pin table**: Template destructor unpins caches but strings themselves might be pinned?
3. **Global/threadvar accumulation**: Many threadvars exist but none obviously accumulate cross-iteration
4. **Allocator not releasing to OS**: Disproven by Copy() test showing proper release

### Next Steps Required
User specifically requested: "prove it with heaptrc or counters"

**Option 1: Heap Tracing** (attempted, hit compilation issues)
- Build with `-gh` flag for heaptrc
- Run small reproduction (3 iterations, 1KB literal)
- Analyze heap.log for unreleased allocations

**Option 2: Runtime Instrumentation**  
- Add allocation/free counters to:
  - TGocciaFunctionTemplate.Create/Destroy
  - TGocciaBytecodeModule.Create/Destroy  
  - FConstants array operations
- Track string sizes in constant pool
- Log at each iteration to see what's accumulating

**Option 3: Valgrind/Massif**
- Profile actual memory allocations
- Identify retention points

## Files Modified
- `source/app/GocciaMemHost.dpr` - Memory profiling host (new)
- `repro_literal.js` - Literal reproduction case (new)
- `repro_runtime.js` - Runtime baseline (new)
