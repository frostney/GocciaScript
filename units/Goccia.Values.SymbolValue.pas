unit Goccia.Values.SymbolValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel.Types,
  Goccia.Values.Primitives;

type
  TGocciaSymbolValue = class(TGocciaValue)
  private class var
    FSharedPrototype: TGocciaValue;
    FMethodHost: TGocciaSymbolValue;
    FPrototypeMembers: array of TGocciaMemberDefinition;
    FWellKnownIterator: TGocciaSymbolValue;
    FWellKnownMatch: TGocciaSymbolValue;
    FWellKnownMatchAll: TGocciaSymbolValue;
    FWellKnownReplace: TGocciaSymbolValue;
    FWellKnownSearch: TGocciaSymbolValue;
    FWellKnownSplit: TGocciaSymbolValue;
    FWellKnownSpecies: TGocciaSymbolValue;
    FWellKnownHasInstance: TGocciaSymbolValue;
    FWellKnownToPrimitive: TGocciaSymbolValue;
    FWellKnownToStringTag: TGocciaSymbolValue;
    FWellKnownIsConcatSpreadable: TGocciaSymbolValue;
    FWellKnownAsyncIterator: TGocciaSymbolValue;
    FWellKnownMetadata: TGocciaSymbolValue;
  private
    FDescription: string;
    FId: Integer;

  public
    constructor Create(const ADescription: string = '');
    procedure InitializePrototype;

    class function SharedPrototype: TGocciaValue;
    class function WellKnownIterator: TGocciaSymbolValue;
    class function WellKnownMatch: TGocciaSymbolValue;
    class function WellKnownMatchAll: TGocciaSymbolValue;
    class function WellKnownReplace: TGocciaSymbolValue;
    class function WellKnownSearch: TGocciaSymbolValue;
    class function WellKnownSplit: TGocciaSymbolValue;
    class function WellKnownSpecies: TGocciaSymbolValue;
    class function WellKnownHasInstance: TGocciaSymbolValue;
    class function WellKnownToPrimitive: TGocciaSymbolValue;
    class function WellKnownToStringTag: TGocciaSymbolValue;
    class function WellKnownIsConcatSpreadable: TGocciaSymbolValue;
    class function WellKnownAsyncIterator: TGocciaSymbolValue;
    class function WellKnownMetadata: TGocciaSymbolValue;
    class function ById(const AId: Integer): TGocciaSymbolValue;

    function TypeName: string; override;
    function TypeOf: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Description: string read FDescription;
    property Id: Integer read FId;
  published
    function GetDescription(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SymbolToString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  GarbageCollector.Generic,
  HashMap,

  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.ObjectModel,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

var
  GNextSymbolId: Integer = 0;
  GSymbolRegistry: THashMap<Integer, TGocciaSymbolValue> = nil;

function TGocciaSymbolValue.SymbolToString(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.prototype.toString requires that ''this'' be a Symbol');
  Result := AThisValue.ToStringLiteral;
end;

function TGocciaSymbolValue.GetDescription(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaSymbolValue) then
    ThrowTypeError('Symbol.prototype.description requires that ''this'' be a Symbol');
  if TGocciaSymbolValue(AThisValue).FDescription <> '' then
    Result := TGocciaStringLiteralValue.Create(TGocciaSymbolValue(AThisValue).FDescription)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaSymbolValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Proto: TGocciaObjectValue;
begin
  if Assigned(FSharedPrototype) then Exit;

  Proto := TGocciaObjectValue.Create;
  FSharedPrototype := Proto;
  FMethodHost := Self;

  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_DESCRIPTION, GetDescription, nil, [pfConfigurable]);
      Members.AddMethod(SymbolToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
    FPrototypeMembers[1].ExposedName := PROP_TO_STRING;
  end;

  RegisterMemberDefinitions(Proto, FPrototypeMembers);

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FSharedPrototype);
    TGarbageCollector.Instance.PinObject(FMethodHost);
  end;
end;

class function TGocciaSymbolValue.SharedPrototype: TGocciaValue;
begin
  Result := FSharedPrototype;
end;

class function TGocciaSymbolValue.WellKnownIterator: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownIterator) then
  begin
    FWellKnownIterator := TGocciaSymbolValue.Create('Symbol.iterator');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownIterator);
  end;
  Result := FWellKnownIterator;
end;

class function TGocciaSymbolValue.WellKnownMatch: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownMatch) then
  begin
    FWellKnownMatch := TGocciaSymbolValue.Create('Symbol.match');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownMatch);
  end;
  Result := FWellKnownMatch;
end;

class function TGocciaSymbolValue.WellKnownMatchAll: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownMatchAll) then
  begin
    FWellKnownMatchAll := TGocciaSymbolValue.Create('Symbol.matchAll');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownMatchAll);
  end;
  Result := FWellKnownMatchAll;
end;

class function TGocciaSymbolValue.WellKnownReplace: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownReplace) then
  begin
    FWellKnownReplace := TGocciaSymbolValue.Create('Symbol.replace');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownReplace);
  end;
  Result := FWellKnownReplace;
end;

class function TGocciaSymbolValue.WellKnownSearch: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownSearch) then
  begin
    FWellKnownSearch := TGocciaSymbolValue.Create('Symbol.search');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownSearch);
  end;
  Result := FWellKnownSearch;
end;

class function TGocciaSymbolValue.WellKnownSplit: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownSplit) then
  begin
    FWellKnownSplit := TGocciaSymbolValue.Create('Symbol.split');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownSplit);
  end;
  Result := FWellKnownSplit;
end;

class function TGocciaSymbolValue.WellKnownSpecies: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownSpecies) then
  begin
    FWellKnownSpecies := TGocciaSymbolValue.Create('Symbol.species');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownSpecies);
  end;
  Result := FWellKnownSpecies;
end;

class function TGocciaSymbolValue.WellKnownHasInstance: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownHasInstance) then
  begin
    FWellKnownHasInstance := TGocciaSymbolValue.Create('Symbol.hasInstance');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownHasInstance);
  end;
  Result := FWellKnownHasInstance;
end;

class function TGocciaSymbolValue.WellKnownToPrimitive: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownToPrimitive) then
  begin
    FWellKnownToPrimitive := TGocciaSymbolValue.Create('Symbol.toPrimitive');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownToPrimitive);
  end;
  Result := FWellKnownToPrimitive;
end;

class function TGocciaSymbolValue.WellKnownToStringTag: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownToStringTag) then
  begin
    FWellKnownToStringTag := TGocciaSymbolValue.Create('Symbol.toStringTag');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownToStringTag);
  end;
  Result := FWellKnownToStringTag;
end;

class function TGocciaSymbolValue.WellKnownIsConcatSpreadable: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownIsConcatSpreadable) then
  begin
    FWellKnownIsConcatSpreadable := TGocciaSymbolValue.Create('Symbol.isConcatSpreadable');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownIsConcatSpreadable);
  end;
  Result := FWellKnownIsConcatSpreadable;
end;

// ES2026 §20.1.2.1 Symbol.asyncIterator
class function TGocciaSymbolValue.WellKnownAsyncIterator: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownAsyncIterator) then
  begin
    FWellKnownAsyncIterator := TGocciaSymbolValue.Create('Symbol.asyncIterator');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownAsyncIterator);
  end;
  Result := FWellKnownAsyncIterator;
end;

// TC39 proposal-decorator-metadata §2 Symbol.metadata
class function TGocciaSymbolValue.WellKnownMetadata: TGocciaSymbolValue;
begin
  if not Assigned(FWellKnownMetadata) then
  begin
    FWellKnownMetadata := TGocciaSymbolValue.Create('Symbol.metadata');
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.PinObject(FWellKnownMetadata);
  end;
  Result := FWellKnownMetadata;
end;

constructor TGocciaSymbolValue.Create(const ADescription: string);
begin
  inherited Create;
  FDescription := ADescription;
  FId := GNextSymbolId;
  Inc(GNextSymbolId);
  if not Assigned(GSymbolRegistry) then
    GSymbolRegistry := THashMap<Integer, TGocciaSymbolValue>.Create;
  GSymbolRegistry.AddOrSetValue(FId, Self);
end;

class function TGocciaSymbolValue.ById(const AId: Integer): TGocciaSymbolValue;
begin
  if Assigned(GSymbolRegistry) and GSymbolRegistry.TryGetValue(AId, Result) then
    Exit;
  Result := nil;
end;

function TGocciaSymbolValue.TypeName: string;
begin
  Result := SYMBOL_TYPE_NAME;
end;

function TGocciaSymbolValue.TypeOf: string;
begin
  Result := SYMBOL_TYPE_NAME;
end;

function TGocciaSymbolValue.GetProperty(const AName: string): TGocciaValue;
begin
  if Assigned(FSharedPrototype) then
  begin
    Result := TGocciaObjectValue(FSharedPrototype).GetPropertyWithContext(AName, Self);
    if Assigned(Result) then
      Exit;
  end;
  Result := nil;
end;

function TGocciaSymbolValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaSymbolValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  ThrowTypeError('Cannot convert a Symbol value to a number');
  Result := nil;
end;

function TGocciaSymbolValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if FDescription <> '' then
    Result := TGocciaStringLiteralValue.Create('Symbol(' + FDescription + ')')
  else
    Result := TGocciaStringLiteralValue.Create('Symbol()');
end;

end.
