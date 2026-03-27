unit Goccia.Values.EnumValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaEnumValue = class(TGocciaObjectValue)
  private
    FName: string;
    FEntries: TGocciaArrayValue;
    function EnumSymbolIterator(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string);
    procedure MarkReferences; override;
    function TypeName: string; override;
    property Name: string read FName;
    property Entries: TGocciaArrayValue read FEntries write FEntries;
  end;

procedure InitializeEnumSymbols(const AEnum: TGocciaEnumValue);

implementation

uses
  GarbageCollector.Generic,

  Goccia.Values.Iterator.Concrete,
  Goccia.Values.SymbolValue;

constructor TGocciaEnumValue.Create(const AName: string);
begin
  inherited Create(nil);
  FName := AName;
  FEntries := nil;
end;

procedure TGocciaEnumValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FEntries) then
    FEntries.MarkReferences;
end;

function TGocciaEnumValue.TypeName: string;
begin
  Result := FName;
end;

// TC39 proposal-enum: Symbol.iterator yields [key, value] entries in declaration order
function TGocciaEnumValue.EnumSymbolIterator(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaArrayIteratorValue.Create(FEntries, akValues);
end;

procedure InitializeEnumSymbols(const AEnum: TGocciaEnumValue);
var
  Members: TGocciaMemberCollection;
  ToStringTag: TGocciaStringLiteralValue;
begin
  Members := TGocciaMemberCollection.Create;
  ToStringTag := TGocciaStringLiteralValue.Create(AEnum.Name);
  try
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(ToStringTag);
    try
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator, '[Symbol.iterator]',
        AEnum.EnumSymbolIterator, 0, []);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        ToStringTag, []);
      RegisterMemberDefinitions(AEnum, Members.ToDefinitions);
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(ToStringTag);
    end;
  finally
    Members.Free;
  end;
end;

end.
