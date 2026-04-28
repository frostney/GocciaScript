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

  TGocciaMatchEnvironment = class
  private
    FScope: TGocciaScope;
    FBindings: TGocciaPatternBindingList;
  public
    constructor Create(const AParentScope: TGocciaScope);
    destructor Destroy; override;
    procedure Bind(const AName: string; const AValue: TGocciaValue;
      const ADeclarationType: TGocciaDeclarationType);
    property Scope: TGocciaScope read FScope;
    property Bindings: TGocciaPatternBindingList read FBindings;
  end;

function MatchValueEquals(const ASubject, ACandidate: TGocciaValue): Boolean;
function GetCustomMatcher(const AMatcher: TGocciaValue): TGocciaValue;
procedure ThrowNoMatchingPattern;

implementation

uses
  Goccia.Evaluator.Comparison,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

{ TGocciaMatchEnvironment }

constructor TGocciaMatchEnvironment.Create(const AParentScope: TGocciaScope);
begin
  inherited Create;
  FScope := AParentScope.CreateChild(skBlock, 'PatternMatchScope');
  FBindings := TGocciaPatternBindingList.Create;
end;

destructor TGocciaMatchEnvironment.Destroy;
begin
  FBindings.Free;
  FScope.Free;
  inherited;
end;

procedure TGocciaMatchEnvironment.Bind(const AName: string; const AValue: TGocciaValue;
  const ADeclarationType: TGocciaDeclarationType);
var
  Binding: TGocciaPatternBinding;
begin
  FScope.DefineLexicalBinding(AName, AValue, ADeclarationType);
  Binding.Name := AName;
  Binding.Value := AValue;
  Binding.DeclarationType := ADeclarationType;
  FBindings.Add(Binding);
end;

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
