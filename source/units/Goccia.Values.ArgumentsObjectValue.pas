unit Goccia.Values.ArgumentsObjectValue;

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
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TArgumentsThrowerHost = class
  public
    function ThrowTypeError(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

var
  GArgumentsThrowerHost: TArgumentsThrowerHost;

function TArgumentsThrowerHost.ThrowTypeError(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Goccia.Values.ErrorHelper.ThrowTypeError(
    '''caller'', ''callee'', and ''arguments'' properties may not be accessed');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function CreateThrowTypeErrorFunction: TGocciaNativeFunctionValue;
begin
  if not Assigned(GArgumentsThrowerHost) then
    GArgumentsThrowerHost := TArgumentsThrowerHost.Create;
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    GArgumentsThrowerHost.ThrowTypeError, 'ThrowTypeError', 0);
end;

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
  Thrower: TGocciaNativeFunctionValue;
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

  Thrower := CreateThrowTypeErrorFunction;
  Result.DefineProperty(PROP_CALLEE,
    TGocciaPropertyDescriptorAccessor.Create(
      Thrower,
      Thrower,
      []));
end;

finalization
  GArgumentsThrowerHost.Free;

end.
