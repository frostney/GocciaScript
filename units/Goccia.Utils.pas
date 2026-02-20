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

implementation

uses
  Math;

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

end.
