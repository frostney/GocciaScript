unit Goccia.PatternMatching;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.Primitives;

type
  TGocciaMatchFailureReason = (
    mfrNone,
    mfrValue,
    mfrStructure,
    mfrGuard,
    mfrCustomMatcher
  );

  TGocciaPatternBinding = record
    Name: string;
    Value: TGocciaValue;
    DeclarationType: TGocciaDeclarationType;
  end;

  TGocciaPatternBindingList = TList<TGocciaPatternBinding>;

function MatchValueEquals(const ASubject, ACandidate: TGocciaValue): Boolean;
function GetCustomMatcher(const AMatcher: TGocciaValue): TGocciaValue;
procedure ThrowNoMatchingPattern;

implementation

uses
  Goccia.Arithmetic,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

// TC39 Pattern Matching: primitive/value patterns use SameValue, except bare
// zero patterns which the parser marks for SameValueZero handling.
function MatchValueEquals(const ASubject, ACandidate: TGocciaValue): Boolean;
begin
  Result := IsSameValue(ASubject, ACandidate);
end;

// TC39 Pattern Matching: Get @@customMatcher from ordinary objects and class
// values. Missing matchers are represented as nil so callers can fall back to
// value equality, predicate functions, or class matching.
function GetCustomMatcher(const AMatcher: TGocciaValue): TGocciaValue;
begin
  Result := nil;
  if AMatcher is TGocciaClassValue then
    Result := TGocciaClassValue(AMatcher).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownCustomMatcher)
  else if AMatcher is TGocciaObjectValue then
    Result := TGocciaObjectValue(AMatcher).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownCustomMatcher);
  if Assigned(Result) and (Result is TGocciaUndefinedLiteralValue) then
    Result := nil;
end;

procedure ThrowNoMatchingPattern;
begin
  ThrowTypeError('No pattern matched');
end;

end.
