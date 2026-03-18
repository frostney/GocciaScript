# FreePascal String Performance

AnsiString vs C, string builder strategies, and FPC heap manager analysis

## FPC AnsiString vs C

Benchmarked with FPC 3.2.2 (-O3) and GCC 13.3.0 (-O3) on Linux x86_64, 5M iterations. C tested with stack-allocated and heap-allocated (malloc/free) equivalents to match AnsiString's heap semantics.

| Benchmark | FPC | C (heap) | C (stack) | Winner | Margin |
|---|---|---|---|---|---|
| Concat short | 3 ms | 34 ms | 3 ms | FPC | 11x vs heap |
| Compare (eq) | 5 ms | 7 ms | — | ~Tie | comparable |
| Assign | 4 ms | 62 ms | 2 ms | FPC | 15x vs heap |
| LowerCase | 540 ms | 102 ms | 67 ms | C | 5–8x |
| Format | 272 ms | 55 ms | — | C | 5x |
| IntToStr | 190 ms | 134 ms | — | C | 1.4x |

**Key finding:** AnsiString's COW semantics make assignment effectively free — a refcount bump vs a full memcpy + malloc in C. The losses are in RTL library functions (LowerCase, Format, Pos) where glibc is more heavily optimised, not in the string type itself.

## Building Strings: Benchmark Results

Six approaches compared: plain concatenation, TStringBuilder (Unicode, the default alias), TAnsiStringBuilder, preallocated variants of both, a custom record-based TStringBuffer, and raw SetLength + direct writes.

### Single-character appends (all preallocated)

| Benchmark | Concat | TSB (U) | TSB (Ansi) | TStringBuffer | Raw |
|---|---|---|---|---|---|
| 50ch, 100K | 95 ms | 168 ms | 158 ms | 23 ms | 6 ms |
| 200ch, 50K | 177 ms | 331 ms | 302 ms | 18 ms | 9.5 ms |
| 1000ch, 10K | 2431 ms | 314 ms | 330 ms | 19 ms | 11 ms |
| 10000ch, 1K | 456 ms | 315 ms | 295 ms | 12 ms | 9 ms |

### Multi-character (word) appends

| Benchmark | Concat | TSB(U) | TSB(U) pre | TSB(A) | TSB(A) pre | TStringBuffer | Raw |
|---|---|---|---|---|---|---|---|
| 50w, 100K | 102 ms | ~45 s | 53 ms | ~44 s | 77 ms | 31 ms | 23 ms |
| 500w, 10K | 1846 ms | 87 ms | 51 ms | 54 ms | 54 ms | 25 ms | 16 ms |

Preallocation is critical for TStringBuilder: 45 seconds drops to 53ms — a **750x improvement**. Even preallocated, TStringBuffer is still ~2x faster due to avoiding virtual dispatch, bounds checks, and class allocation overhead.

## TStringBuilder Pathology: Root Cause

Initial suspicion was Unicode conversion overhead, since TStringBuilder is aliased to TUnicodeStringBuilder in the RTL (`FData` is `Array of WideChar`, every AnsiString append triggers a conversion). However, TAnsiStringBuilder shows the identical ~44 second pathology, ruling out Unicode conversion.

### General FPC heap manager limitation

A minimal reproduction using only SetLength growth on bare dynamic arrays (no string operations, no TStringBuilder) reproduces the same multi-second timings. This is a general FPC default heap manager limitation with repeated alloc→realloc→realloc→free cycles:

| Grow from 64 to… | Dynamic Array | AnsiString | Prealloc (no grow) |
|---|---|---|---|
| 256 (10K rounds) | 4,324 ms | 4,644 ms | 0 ms |
| 512 (10K rounds) | 4,392 ms | 4,616 ms | 0.5 ms |
| 1024 (10K rounds) | 4,371 ms | 4,626 ms | 1 ms |
| 4096 (10K rounds) | 4,394 ms | 4,552 ms | 1 ms |

Both dynamic arrays and AnsiStrings are equally affected. Preallocation eliminates the issue entirely (0–1ms vs 4,000+ms). The pathology appears consistent regardless of target size — even growing from 64 to 256 (just 2 reallocs) takes ~4.3 seconds over 10K rounds. This suggests the FPC memory manager's free-list or coalescing logic degrades under rapid small-grow-free cycles.

## TStringBuffer Implementation

A minimal advanced record wrapper around SetLength + Move with doubling growth. Uses a class constructor, a Length property, and AnsiString as the internal buffer (avoiding the dynamic array heap pathology for typical prealloc usage). Within 1.1–1.7x of raw buffer writes.

### Type declaration

```pascal
type
  TStringBuffer = record
  private
    FData: AnsiString;
    FLen: Integer;
    FCap: Integer;
    function GetLength: Integer; inline;
  public
    class function Create(ACapacity: Integer = 64): TStringBuffer;
      static; inline;
    procedure Append(const S: AnsiString); inline;
    procedure AppendChar(C: AnsiChar); inline;
    function ToString: AnsiString;
    property Length: Integer read GetLength;
  end;
```

### Core methods

```pascal
class function TStringBuffer.Create(ACapacity: Integer): TStringBuffer;
begin
  Result.FLen := 0;
  Result.FCap := ACapacity;
  SetLength(Result.FData, Result.FCap);
end;

procedure TStringBuffer.AppendChar(C: AnsiChar); inline;
begin
  if FLen + 1 > FCap then begin
    FCap := FCap * 2;
    SetLength(FData, FCap);
  end;
  Inc(FLen);
  FData[FLen] := C;
end;

procedure TStringBuffer.Append(const S: AnsiString); inline;
var
  SLen, NewCap: Integer;
begin
  SLen := System.Length(S);
  if SLen = 0 then Exit;
  if FLen + SLen > FCap then begin
    NewCap := FCap;
    while NewCap < FLen + SLen do NewCap := NewCap * 2;
    FCap := NewCap;
    SetLength(FData, FCap);
  end;
  Move(S[1], FData[FLen + 1], SLen);
  Inc(FLen, SLen);
end;

function TStringBuffer.ToString: AnsiString;
begin
  Result := Copy(FData, 1, FLen);
end;
```

### Usage

```pascal
var
  buf: TStringBuffer;
begin
  buf := TStringBuffer.Create(256);
  buf.Append('function ');
  buf.Append(FuncName);
  buf.AppendChar('(');
  buf.Append(Args);
  buf.AppendChar(')');
  Result := buf.ToString; // buf.Length available via property
end;
```

## Summary

AnsiString is **not** slow. COW makes assignment and short concatenation cheaper than C. String interning does not help in FPC (tested six times, typically 2–3% regression). The losses vs C are in RTL library functions, not the string type.

For string building, **TStringBuffer** gives near-raw performance with a clean API. **TStringBuilder should be avoided** without preallocation: both the Unicode and Ansi variants trigger a pathological ~750x slowdown from FPC's default heap manager struggling with repeated grow-free cycles. This is a general FPC heap limitation, not specific to TStringBuilder — bare dynamic arrays and AnsiStrings exhibit the same behaviour. Preallocation eliminates it, but TStringBuffer remains ~2x faster even then, due to avoiding virtual method dispatch and per-call bounds checking.

**Environment:** FPC 3.2.2 / GCC 13.3.0, Linux x86_64, -O3. All timings averaged across multiple runs.
