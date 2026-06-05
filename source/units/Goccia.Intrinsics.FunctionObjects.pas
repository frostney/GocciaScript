unit Goccia.Intrinsics.FunctionObjects;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Constants,
  Goccia.Values.ClassValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFunctionObjectIntrinsicKind = (
    foikOrdinary,
    foikAsync,
    foikGenerator,
    foikAsyncGenerator
  );

  TGocciaFunctionObjectIntrinsics = class
  private
    FAsyncFunctionConstructor: TGocciaFunctionConstructorClassValue;
    FAsyncFunctionPrototype: TGocciaObjectValue;
    FGeneratorFunctionConstructor: TGocciaFunctionConstructorClassValue;
    FGeneratorFunctionPrototype: TGocciaObjectValue;
    FGeneratorPrototype: TGocciaObjectValue;
    FAsyncIteratorPrototype: TGocciaObjectValue;
    FAsyncGeneratorFunctionConstructor: TGocciaFunctionConstructorClassValue;
    FAsyncGeneratorFunctionPrototype: TGocciaObjectValue;
    FAsyncGeneratorPrototype: TGocciaObjectValue;

    function AsyncIteratorSelf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CreateHiddenFunctionConstructor(const AName: string;
      const AFunctionPrototype, AConstructorPrototype: TGocciaObjectValue;
      const AKind: TGocciaDynamicFunctionKind):
      TGocciaFunctionConstructorClassValue;
    procedure DefineToStringTag(const AObject: TGocciaObjectValue;
      const ATag: string);
    procedure EnsureAsyncFunctionPrototype(
      const AFunctionPrototype: TGocciaObjectValue);
    procedure EnsureGeneratorIntrinsics(
      const AFunctionPrototype, AIteratorPrototype: TGocciaObjectValue);
    procedure EnsureAsyncGeneratorIntrinsics(
      const AFunctionPrototype, AObjectPrototype: TGocciaObjectValue);
  public
    class function Ensure: TGocciaFunctionObjectIntrinsics; static;

    function FunctionPrototypeFor(
      const AKind: TGocciaFunctionObjectIntrinsicKind;
      const AFunctionPrototype, AObjectPrototype,
      AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
    function GeneratorObjectPrototypeFor(
      const AKind: TGocciaFunctionObjectIntrinsicKind;
      const AFunctionPrototype, AObjectPrototype,
      AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
  end;

function FunctionObjectIntrinsicPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const AFunctionPrototype, AObjectPrototype,
  AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
function GeneratorObjectIntrinsicPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const AFunctionPrototype, AObjectPrototype,
  AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

var
  GFunctionObjectIntrinsicsSlot: TGocciaRealmOwnedSlotId;
  GAsyncFunctionPrototypeSlot: TGocciaRealmSlotId;
  GGeneratorFunctionPrototypeSlot: TGocciaRealmSlotId;
  GGeneratorPrototypeSlot: TGocciaRealmSlotId;
  GAsyncIteratorPrototypeSlot: TGocciaRealmSlotId;
  GAsyncGeneratorFunctionPrototypeSlot: TGocciaRealmSlotId;
  GAsyncGeneratorPrototypeSlot: TGocciaRealmSlotId;

procedure RequirePrototype(const APrototype: TGocciaObjectValue;
  const AName: string);
begin
  if not Assigned(APrototype) then
    raise Exception.Create(AName + ' is required to initialize function object intrinsics');
end;

{ TGocciaFunctionObjectIntrinsics }

class function TGocciaFunctionObjectIntrinsics.Ensure: TGocciaFunctionObjectIntrinsics;
begin
  if not Assigned(CurrentRealm) then
    raise Exception.Create('Function object intrinsics require an active realm');

  Result := TGocciaFunctionObjectIntrinsics(
    CurrentRealm.GetOwnedSlot(GFunctionObjectIntrinsicsSlot));
  if Assigned(Result) then
    Exit;

  Result := TGocciaFunctionObjectIntrinsics.Create;
  CurrentRealm.SetOwnedSlot(GFunctionObjectIntrinsicsSlot, Result);
end;

function TGocciaFunctionObjectIntrinsics.AsyncIteratorSelf(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

function TGocciaFunctionObjectIntrinsics.CreateHiddenFunctionConstructor(
  const AName: string;
  const AFunctionPrototype, AConstructorPrototype: TGocciaObjectValue;
  const AKind: TGocciaDynamicFunctionKind):
  TGocciaFunctionConstructorClassValue;
var
  FunctionConstructorValue: TGocciaValue;
  FunctionConstructor: TGocciaFunctionConstructorClassValue;
begin
  RequirePrototype(AFunctionPrototype, '%Function.prototype%');
  RequirePrototype(AConstructorPrototype, '%' + AName + '.prototype%');

  Result := TGocciaFunctionConstructorClassValue.Create(AName, nil, AKind);
  Result.SetConstructorPrototype(AFunctionPrototype);
  Result.ReplacePrototype(AConstructorPrototype);

  FunctionConstructorValue := AFunctionPrototype.GetProperty(PROP_CONSTRUCTOR);
  if FunctionConstructorValue is TGocciaFunctionConstructorClassValue then
  begin
    FunctionConstructor := TGocciaFunctionConstructorClassValue(
      FunctionConstructorValue);
    Result.Enabled := FunctionConstructor.Enabled;
    Result.CompileDynamicFunction := FunctionConstructor.CompileDynamicFunction;
  end;
end;

procedure TGocciaFunctionObjectIntrinsics.DefineToStringTag(
  const AObject: TGocciaObjectValue; const ATag: string);
begin
  AObject.DefineSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag,
    TGocciaPropertyDescriptorData.Create(TGocciaStringLiteralValue.Create(ATag),
      [pfConfigurable]));
end;

procedure TGocciaFunctionObjectIntrinsics.EnsureAsyncFunctionPrototype(
  const AFunctionPrototype: TGocciaObjectValue);
var
  ConstructorFunction: TGocciaFunctionConstructorClassValue;
  Prototype: TGocciaObjectValue;
begin
  if Assigned(FAsyncFunctionPrototype) then
    Exit;

  RequirePrototype(AFunctionPrototype, '%Function.prototype%');
  Prototype := TGocciaObjectValue(
    CurrentRealm.GetSlot(GAsyncFunctionPrototypeSlot));
  if not Assigned(Prototype) then
  begin
    Prototype := TGocciaObjectValue.Create(AFunctionPrototype);
    ConstructorFunction := CreateHiddenFunctionConstructor('AsyncFunction',
      AFunctionPrototype, Prototype, dfkAsync);
    Prototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(ConstructorFunction, [pfConfigurable]));
    DefineToStringTag(Prototype, 'AsyncFunction');
    CurrentRealm.SetSlot(GAsyncFunctionPrototypeSlot, Prototype);
  end
  else
  begin
    ConstructorFunction := Prototype.GetProperty(PROP_CONSTRUCTOR) as
      TGocciaFunctionConstructorClassValue;
  end;
  FAsyncFunctionConstructor := ConstructorFunction;
  FAsyncFunctionPrototype := Prototype;
end;

procedure TGocciaFunctionObjectIntrinsics.EnsureGeneratorIntrinsics(
  const AFunctionPrototype, AIteratorPrototype: TGocciaObjectValue);
var
  ConstructorFunction: TGocciaFunctionConstructorClassValue;
  FunctionPrototype: TGocciaObjectValue;
  GeneratorPrototype: TGocciaObjectValue;
begin
  if Assigned(FGeneratorFunctionPrototype) and Assigned(FGeneratorPrototype) then
    Exit;

  RequirePrototype(AFunctionPrototype, '%Function.prototype%');
  RequirePrototype(AIteratorPrototype, '%Iterator.prototype%');
  FunctionPrototype := TGocciaObjectValue(
    CurrentRealm.GetSlot(GGeneratorFunctionPrototypeSlot));
  GeneratorPrototype := TGocciaObjectValue(
    CurrentRealm.GetSlot(GGeneratorPrototypeSlot));
  if not Assigned(FunctionPrototype) or not Assigned(GeneratorPrototype) then
  begin
    FunctionPrototype := TGocciaObjectValue.Create(AFunctionPrototype);
    GeneratorPrototype := TGocciaObjectValue.Create(AIteratorPrototype);
    ConstructorFunction := CreateHiddenFunctionConstructor('GeneratorFunction',
      AFunctionPrototype, FunctionPrototype, dfkGenerator);

    FunctionPrototype.DefineProperty(PROP_PROTOTYPE,
      TGocciaPropertyDescriptorData.Create(GeneratorPrototype,
        [pfConfigurable]));
    FunctionPrototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(ConstructorFunction, [pfConfigurable]));
    DefineToStringTag(FunctionPrototype, 'GeneratorFunction');

    GeneratorPrototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(FunctionPrototype,
        [pfConfigurable]));
    DefineToStringTag(GeneratorPrototype, 'Generator');

    CurrentRealm.SetSlot(GGeneratorFunctionPrototypeSlot, FunctionPrototype);
    CurrentRealm.SetSlot(GGeneratorPrototypeSlot, GeneratorPrototype);
  end
  else
  begin
    ConstructorFunction := FunctionPrototype.GetProperty(PROP_CONSTRUCTOR) as
      TGocciaFunctionConstructorClassValue;
  end;
  FGeneratorFunctionConstructor := ConstructorFunction;
  FGeneratorFunctionPrototype := FunctionPrototype;
  FGeneratorPrototype := GeneratorPrototype;
end;

procedure TGocciaFunctionObjectIntrinsics.EnsureAsyncGeneratorIntrinsics(
  const AFunctionPrototype, AObjectPrototype: TGocciaObjectValue);
var
  ConstructorFunction: TGocciaFunctionConstructorClassValue;
  FunctionPrototype: TGocciaObjectValue;
  AsyncIteratorPrototype: TGocciaObjectValue;
  AsyncGeneratorPrototype: TGocciaObjectValue;
begin
  if Assigned(FAsyncGeneratorFunctionPrototype) and
     Assigned(FAsyncGeneratorPrototype) and
     Assigned(FAsyncIteratorPrototype) then
    Exit;

  RequirePrototype(AFunctionPrototype, '%Function.prototype%');
  RequirePrototype(AObjectPrototype, '%Object.prototype%');
  FunctionPrototype := TGocciaObjectValue(
    CurrentRealm.GetSlot(GAsyncGeneratorFunctionPrototypeSlot));
  AsyncIteratorPrototype := TGocciaObjectValue(
    CurrentRealm.GetSlot(GAsyncIteratorPrototypeSlot));
  AsyncGeneratorPrototype := TGocciaObjectValue(
    CurrentRealm.GetSlot(GAsyncGeneratorPrototypeSlot));
  if not Assigned(FunctionPrototype) or
     not Assigned(AsyncIteratorPrototype) or
     not Assigned(AsyncGeneratorPrototype) then
  begin
    FunctionPrototype := TGocciaObjectValue.Create(AFunctionPrototype);
    AsyncIteratorPrototype := TGocciaObjectValue.Create(AObjectPrototype);
    AsyncGeneratorPrototype := TGocciaObjectValue.Create(
      AsyncIteratorPrototype);
    ConstructorFunction := CreateHiddenFunctionConstructor('AsyncGeneratorFunction',
      AFunctionPrototype, FunctionPrototype, dfkAsyncGenerator);

    AsyncIteratorPrototype.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownAsyncIterator,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          AsyncIteratorSelf, '[Symbol.asyncIterator]', 0),
        [pfConfigurable, pfWritable]));

    FunctionPrototype.DefineProperty(PROP_PROTOTYPE,
      TGocciaPropertyDescriptorData.Create(AsyncGeneratorPrototype,
        [pfConfigurable]));
    FunctionPrototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(ConstructorFunction, [pfConfigurable]));
    DefineToStringTag(FunctionPrototype, 'AsyncGeneratorFunction');

    AsyncGeneratorPrototype.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(FunctionPrototype,
        [pfConfigurable]));
    DefineToStringTag(AsyncGeneratorPrototype, 'AsyncGenerator');

    CurrentRealm.SetSlot(GAsyncGeneratorFunctionPrototypeSlot,
      FunctionPrototype);
    CurrentRealm.SetSlot(GAsyncIteratorPrototypeSlot, AsyncIteratorPrototype);
    CurrentRealm.SetSlot(GAsyncGeneratorPrototypeSlot, AsyncGeneratorPrototype);
  end
  else
  begin
    ConstructorFunction := FunctionPrototype.GetProperty(PROP_CONSTRUCTOR) as
      TGocciaFunctionConstructorClassValue;
  end;
  FAsyncGeneratorFunctionConstructor := ConstructorFunction;
  FAsyncGeneratorFunctionPrototype := FunctionPrototype;
  FAsyncIteratorPrototype := AsyncIteratorPrototype;
  FAsyncGeneratorPrototype := AsyncGeneratorPrototype;
end;

function TGocciaFunctionObjectIntrinsics.FunctionPrototypeFor(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const AFunctionPrototype, AObjectPrototype,
  AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
begin
  case AKind of
    foikOrdinary:
      Result := AFunctionPrototype;
    foikAsync:
      begin
        EnsureAsyncFunctionPrototype(AFunctionPrototype);
        Result := FAsyncFunctionPrototype;
      end;
    foikGenerator:
      begin
        EnsureGeneratorIntrinsics(AFunctionPrototype, AIteratorPrototype);
        Result := FGeneratorFunctionPrototype;
      end;
    foikAsyncGenerator:
      begin
        EnsureAsyncGeneratorIntrinsics(AFunctionPrototype, AObjectPrototype);
        Result := FAsyncGeneratorFunctionPrototype;
      end;
  else
    Result := AFunctionPrototype;
  end;
end;

function TGocciaFunctionObjectIntrinsics.GeneratorObjectPrototypeFor(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const AFunctionPrototype, AObjectPrototype,
  AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
begin
  case AKind of
    foikGenerator:
      begin
        EnsureGeneratorIntrinsics(AFunctionPrototype, AIteratorPrototype);
        Result := FGeneratorPrototype;
      end;
    foikAsyncGenerator:
      begin
        EnsureAsyncGeneratorIntrinsics(AFunctionPrototype, AObjectPrototype);
        Result := FAsyncGeneratorPrototype;
      end;
  else
    Result := AObjectPrototype;
  end;
end;

function FunctionObjectIntrinsicPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const AFunctionPrototype, AObjectPrototype,
  AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := TGocciaFunctionObjectIntrinsics.Ensure.FunctionPrototypeFor(
    AKind, AFunctionPrototype, AObjectPrototype, AIteratorPrototype);
end;

function GeneratorObjectIntrinsicPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const AFunctionPrototype, AObjectPrototype,
  AIteratorPrototype: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := TGocciaFunctionObjectIntrinsics.Ensure.GeneratorObjectPrototypeFor(
    AKind, AFunctionPrototype, AObjectPrototype, AIteratorPrototype);
end;

initialization
  GFunctionObjectIntrinsicsSlot := RegisterRealmOwnedSlot(
    'Function object intrinsics');
  GAsyncFunctionPrototypeSlot := RegisterRealmSlot('AsyncFunction.prototype');
  GGeneratorFunctionPrototypeSlot := RegisterRealmSlot(
    'GeneratorFunction.prototype');
  GGeneratorPrototypeSlot := RegisterRealmSlot('Generator.prototype');
  GAsyncIteratorPrototypeSlot := RegisterRealmSlot('AsyncIterator.prototype');
  GAsyncGeneratorFunctionPrototypeSlot := RegisterRealmSlot(
    'AsyncGeneratorFunction.prototype');
  GAsyncGeneratorPrototypeSlot := RegisterRealmSlot('AsyncGenerator.prototype');

end.
