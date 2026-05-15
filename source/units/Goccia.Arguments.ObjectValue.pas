unit Goccia.Arguments.ObjectValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ObjectValue;

// ES2026 §10.4.4.6 CreateUnmappedArgumentsObject(argumentsList).
function CreateUnmappedArgumentsObject(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

function ArrayPrototypeValuesFunction: TGocciaValue;
var
  ArrayValue: TGocciaArrayValue;
begin
  ArrayValue := TGocciaArrayValue.Create;
  Result := ArrayValue.GetProperty(PROP_VALUES);
end;

function CreateUnmappedArgumentsObject(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  I: Integer;
  IteratorValue: TGocciaValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    AArguments.Length + 2);

  Result.DefineProperty(PROP_LENGTH,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNumberLiteralValue.Create(AArguments.Length),
      [pfWritable, pfConfigurable]));

  for I := 0 to AArguments.Length - 1 do
    Result.DefineProperty(IntToStr(I),
      TGocciaPropertyDescriptorData.Create(
        AArguments.GetElement(I),
        [pfEnumerable, pfWritable, pfConfigurable]));

  IteratorValue := ArrayPrototypeValuesFunction;
  if Assigned(IteratorValue) and not (IteratorValue is TGocciaUndefinedLiteralValue) then
    Result.DefineSymbolProperty(TGocciaSymbolValue.WellKnownIterator,
      TGocciaPropertyDescriptorData.Create(IteratorValue,
        [pfWritable, pfConfigurable]));

  // Keep `callee` present and non-enumerable.  Goccia uses unmapped
  // arguments objects in all functions, so there is no parameter map.
  Result.DefineProperty(PROP_CALLEE,
    TGocciaPropertyDescriptorData.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue,
      []));
end;

end.
