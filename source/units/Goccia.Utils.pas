unit Goccia.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

// ES2026 §7.1.6 ToIntegerOrInfinity — extract integer arg with default.
// Returns Trunc(ToNumber(AArgs[AIndex])) or ADefault if index out of range.
function ToIntegerFromArgs(const AArgs: TGocciaArgumentsCollection; const AIndex: Integer = 0; const ADefault: Integer = 0): Integer; inline;

// 64-bit variant of ToIntegerFromArgs.  Used by Array prototype methods that
// must reach indices above MaxInt when the receiver is a generic array-like
// with a length up to 2^53 - 1 (e.g. test262
// length-near-integer-limit fixtures).  Saturates at ±MAX_SAFE_INTEGER and
// treats NaN as 0, matching ToIntegerOrInfinity's behaviour for the inputs
// these methods accept (no Infinity propagation needed — callers clamp by
// length anyway).
function ToInteger64FromArgs(const AArgs: TGocciaArgumentsCollection; const AIndex: Integer = 0; const ADefault: Int64 = 0): Int64; inline;

// ES2026 §7.1.6 ToInt32(argument)
//   1. Let number be ? ToNumber(argument).
//   2. If number is NaN, +0, -0, +∞, or -∞, return +0.
//   3. Let int be sign(number) × floor(abs(number)).
//   4. Let int32bit be int modulo 2**32.
//   5. If int32bit ≥ 2**31, return int32bit - 2**32; otherwise int32bit.
//
// Bitwise operators MUST route through this helper rather than calling
// Trunc(...) directly: FPC's Trunc(NaN) returns Int64.MinValue on x86_64
// (cvttsd2si "indefinite" sentinel) and 0 on aarch64.  Bare Trunc made
// `(undefined & undefined)` evaluate to ~ -2^63 on Linux x86_64 CI while
// passing 0 on macOS arm64 — see test262 language/expressions/bitwise-*
// /S11.10.*_A3_T1.4.js for the regression cluster this fixes.
function ToInt32Value(const AValue: TGocciaValue): Int32; inline;

// ES2026 §7.1.7 ToUint32(argument)
function ToUint32Value(const AValue: TGocciaValue): Cardinal; inline;

// ES2026 relative index normalization (used by slice, splice, at, with, copyWithin, fill, etc.)
// If ARelative < 0, returns max(ALength + ARelative, 0); else min(ARelative, ALength).
function NormalizeRelativeIndex(const ARelative, ALength: Integer): Integer; inline;

// ES2026 §7.3.14 Call(F, V, argumentsList) — safely invoke any callable value.
// Dispatches through TGocciaFunctionBase.Call or TGocciaClassValue.Call based on runtime type.
function InvokeCallable(const ACallable: TGocciaValue; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue; inline;

implementation

uses
  Math,
  SysUtils,

  Goccia.Constants.NumericLimits,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase;

function ToIntegerFromArgs(const AArgs: TGocciaArgumentsCollection; const AIndex: Integer; const ADefault: Integer): Integer;
var
  NumberValue: TGocciaNumberLiteralValue;
begin
  if AArgs.Length > AIndex then
  begin
    NumberValue := AArgs.GetElement(AIndex).ToNumberLiteral;
    if NumberValue.IsNaN then
      Exit(0);
    if NumberValue.IsInfinity then
      Exit(MaxInt);
    if NumberValue.IsNegativeInfinity then
      Exit(-MaxInt);
    if NumberValue.Value >= MaxInt then
      Exit(MaxInt);
    if NumberValue.Value <= -MaxInt then
      Exit(-MaxInt);
    Result := Trunc(NumberValue.Value);
    Exit;
  end;
  Result := ADefault;
end;

function ToInteger64FromArgs(const AArgs: TGocciaArgumentsCollection; const AIndex: Integer; const ADefault: Int64): Int64;
var
  Number: Double;
begin
  if AArgs.Length <= AIndex then
    Exit(ADefault);

  Number := AArgs.GetElement(AIndex).ToNumberLiteral.Value;
  if IsNan(Number) then
    Result := 0
  else if Number >= MAX_SAFE_INTEGER_F then
    Result := MAX_SAFE_INTEGER
  else if Number <= -MAX_SAFE_INTEGER_F then
    Result := -MAX_SAFE_INTEGER
  else
    Result := Trunc(Number);
end;

function ToUint32Value(const AValue: TGocciaValue): Cardinal;
const
  UINT32_MODULUS = QWord(High(Cardinal)) + 1;
var
  NumberValue: TGocciaNumberLiteralValue;
  IntegerPart: Double;
begin
  NumberValue := AValue.ToNumberLiteral;
  if NumberValue.IsNaN or NumberValue.IsInfinity or
     NumberValue.IsNegativeInfinity or (NumberValue.Value = 0) then
    Exit(0);

  IntegerPart := Int(NumberValue.Value);
  IntegerPart := IntegerPart - Floor(IntegerPart / UINT32_MODULUS) *
    UINT32_MODULUS;

  if IntegerPart >= UINT32_MODULUS then
    Exit(0);

  Result := Cardinal(Trunc(IntegerPart));
end;

function ToInt32Value(const AValue: TGocciaValue): Int32;
const
  INT32_MODULUS = QWord(High(Cardinal)) + 1;          // 2^32
  INT32_HALF_MODULUS = QWord(High(Cardinal)) + 1 - QWord(2147483648); // 2^31, but we use the comparison directly below
var
  AsUint: Cardinal;
begin
  // Reuse ToUint32Value (already NaN/Infinity-safe) and reinterpret the
  // resulting 32-bit pattern as a signed two's-complement Int32 — the
  // last spec step of ToInt32 is exactly "if int32bit ≥ 2^31, subtract
  // 2^32".  Casting Cardinal → Int32 in FPC performs that reinterpretation
  // directly without any FP path that could trip Trunc(NaN).
  AsUint := ToUint32Value(AValue);
  Result := Int32(AsUint);
end;

function NormalizeRelativeIndex(const ARelative, ALength: Integer): Integer;
begin
  if ARelative < 0 then
    Result := Max(ALength + ARelative, 0)
  else
    Result := Min(ARelative, ALength);
end;

function InvokeCallable(const ACallable: TGocciaValue; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if ACallable is TGocciaFunctionBase then
    Result := TGocciaFunctionBase(ACallable).Call(AArgs, AThisValue)
  else if ACallable is TGocciaClassValue then
    Result := TGocciaClassValue(ACallable).Call(AArgs, AThisValue)
  else
    ThrowTypeError(Format('%s is not a function', [ACallable.TypeName]));
end;

end.
