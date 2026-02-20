unit Goccia.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

// ES2026 §7.1.6 ToIntegerOrInfinity — extract integer arg with default.
// Returns Trunc(ToNumber(AArgs[AIndex])) or ADefault if index out of range.
function ToIntegerFromArgs(const AArgs: TGocciaArgumentsCollection; const AIndex: Integer; const ADefault: Integer): Integer; inline;

// ES2026 relative index normalization (used by slice, splice, at, with, copyWithin, fill, etc.)
// If ARelative < 0, returns max(ALength + ARelative, 0); else min(ARelative, ALength).
function NormalizeRelativeIndex(const ARelative, ALength: Integer): Integer; inline;

// ES2026 §7.3.14 Call(F, V, argumentsList) — invoke a callable with args and this value.
// Dispatches through TGocciaFunctionBase.Call virtual method.
function CallFunction(const ACallable: TGocciaValue; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue; inline;

implementation

uses
  Math,

  Goccia.Values.FunctionBase;

// TODO: We should set defaults for index and default value
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

// TODO: Do we need this?
function CallFunction(const ACallable: TGocciaValue; const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaFunctionBase(ACallable).Call(AArgs, AThisValue);
end;

end.
