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
begin
  if AArgs.Length > AIndex then
    Result := Trunc(AArgs.GetElement(AIndex).ToNumberLiteral.Value)
  else
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
