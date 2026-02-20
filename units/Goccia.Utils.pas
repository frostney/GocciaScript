unit Goccia.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

// ES2026 §7.1.6 ToIntegerOrInfinity — extract integer arg with default.
// Returns Trunc(ToNumber(AArgs[AIndex])) or ADefault if index out of range.
function ToIntegerFromArgs(const AArgs: TGocciaArgumentsCollection; const AIndex: Integer = 0; const ADefault: Integer = 0): Integer; inline;

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
