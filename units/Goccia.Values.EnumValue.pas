unit Goccia.Values.EnumValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
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
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
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
begin
  AEnum.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.Create(AEnum.EnumSymbolIterator, '[Symbol.iterator]', 0),
      []
    )
  );

  AEnum.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(AEnum.Name),
      []
    )
  );
end;

end.
