// Generator machinery both executors share: the generator object base
// classes, the next/return/throw methods on the generator prototypes, and the
// generator function value classes the evaluator creates. The interpreter's
// generator objects and the call path that creates them live in
// Goccia.Evaluator.Generators, which registers that path with
// RegisterGeneratorObjectFactory; the bytecode VM's live in Goccia.VM.
unit Goccia.Values.GeneratorValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Intrinsics.FunctionObjects,
  Goccia.Realm,
  Goccia.Values.FunctionValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue;

type
  TGocciaGeneratorBaseValue = class(TGocciaIteratorValue)
  public
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
  end;

  TGocciaAsyncGeneratorBaseValue = class(TGocciaObjectValue)
  public
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; virtual; abstract;
  end;

  { Creates the object a call to AFunction returns: an async generator object
    when AIsAsync, a generator object otherwise. The caller sets its
    prototype. }
  TGocciaGeneratorObjectFactory = function(
    const AFunction: TGocciaFunctionValue;
    const AArguments: TGocciaArgumentsCollection;
    const AThisValue: TGocciaValue;
    const AIsAsync: Boolean): TGocciaObjectValue;

  TGocciaGeneratorFunctionValue = class(TGocciaFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function IsConstructable: Boolean; override;
  end;

  TGocciaAsyncGeneratorFunctionValue = class(TGocciaGeneratorFunctionValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaGeneratorMethodValue = class(TGocciaMethodValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaAsyncGeneratorMethodValue = class(TGocciaGeneratorMethodValue)
  public
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

procedure EnsureGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
procedure EnsureAsyncGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);

{ Registers how the interpreter creates generator objects.
  Goccia.Evaluator.Generators does this when it is linked. }
procedure RegisterGeneratorObjectFactory(
  const AFactory: TGocciaGeneratorObjectFactory);

{ The realm's %GeneratorPrototype% or %AsyncGeneratorPrototype%, created on
  first use. ARealm defaults to the current realm. }
function GeneratorObjectIntrinsicParent(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const ARealm: TGocciaRealm = nil): TGocciaObjectValue;
function GeneratorIncompatibleReceiverError: TGocciaValue;
function RejectedTypeErrorPromise(const AMessage: string): TGocciaPromiseValue;

implementation

uses
  SysUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

type
  TGocciaGeneratorPrototypeMethodHost = class
  public
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAsyncGeneratorPrototypeMethodHost = class
  public
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

var
  GGeneratorPrototypeMethodHostSlot: TGocciaRealmOwnedSlotId;
  GAsyncGeneratorPrototypeMethodHostSlot: TGocciaRealmOwnedSlotId;

function GeneratorObjectIntrinsicParent(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const ARealm: TGocciaRealm = nil): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
  PreviousRealm: TGocciaRealm;
  ShouldSwitchRealm: Boolean;
begin
  PreviousRealm := CurrentRealm;
  ShouldSwitchRealm := Assigned(ARealm) and (ARealm <> PreviousRealm);
  if ShouldSwitchRealm then
    SetCurrentRealm(ARealm);
  try
    if TGocciaObjectValue.SharedObjectPrototype = nil then
      TGocciaObjectValue.InitializeSharedPrototype;
    IteratorPrototype := nil;
    if AKind = foikGenerator then
      IteratorPrototype := TGocciaIteratorValue.SharedPrototype;
    Result := GeneratorObjectIntrinsicPrototype(AKind,
      TGocciaFunctionBase.GetSharedPrototype,
      TGocciaObjectValue.SharedObjectPrototype,
      IteratorPrototype);
    if AKind = foikGenerator then
      EnsureGeneratorPrototypeMethods(Result)
    else if AKind = foikAsyncGenerator then
      EnsureAsyncGeneratorPrototypeMethods(Result);
  finally
    if ShouldSwitchRealm then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function GeneratorIncompatibleReceiverError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME,
    'Generator method called on incompatible receiver');
end;

function RejectedTypeErrorPromise(const AMessage: string): TGocciaPromiseValue;
begin
  Result := TGocciaPromiseValue.Create;
  try
    Result.Reject(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
  except
    Result.Free;
    raise;
  end;
end;

function GeneratorPrototypeMethodHost: TGocciaGeneratorPrototypeMethodHost;
begin
  if (CurrentRealm = nil) then
    raise Exception.Create('Generator prototype methods require an active realm');
  Result := TGocciaGeneratorPrototypeMethodHost(
    CurrentRealm.GetOwnedSlot(GGeneratorPrototypeMethodHostSlot));
  if Assigned(Result) then
    Exit;
  Result := TGocciaGeneratorPrototypeMethodHost.Create;
  CurrentRealm.SetOwnedSlot(GGeneratorPrototypeMethodHostSlot, Result);
end;

function AsyncGeneratorPrototypeMethodHost: TGocciaAsyncGeneratorPrototypeMethodHost;
begin
  if (CurrentRealm = nil) then
    raise Exception.Create('AsyncGenerator prototype methods require an active realm');
  Result := TGocciaAsyncGeneratorPrototypeMethodHost(
    CurrentRealm.GetOwnedSlot(GAsyncGeneratorPrototypeMethodHostSlot));
  if Assigned(Result) then
    Exit;
  Result := TGocciaAsyncGeneratorPrototypeMethodHost.Create;
  CurrentRealm.SetOwnedSlot(GAsyncGeneratorPrototypeMethodHostSlot, Result);
end;

function TGocciaGeneratorPrototypeMethodHost.GeneratorNext(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorBaseValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  Result := TGocciaGeneratorBaseValue(AThisValue).GeneratorNext(AArgs,
    AThisValue);
end;

function TGocciaGeneratorPrototypeMethodHost.GeneratorReturn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorBaseValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  Result := TGocciaGeneratorBaseValue(AThisValue).GeneratorReturn(AArgs,
    AThisValue);
end;

function TGocciaGeneratorPrototypeMethodHost.GeneratorThrow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaGeneratorBaseValue) then
    raise TGocciaThrowValue.Create(GeneratorIncompatibleReceiverError);
  Result := TGocciaGeneratorBaseValue(AThisValue).GeneratorThrow(AArgs,
    AThisValue);
end;

function TGocciaAsyncGeneratorPrototypeMethodHost.AsyncGeneratorNext(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorBaseValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorBaseValue(AThisValue).AsyncGeneratorNext(
    AArgs, AThisValue);
end;

function TGocciaAsyncGeneratorPrototypeMethodHost.AsyncGeneratorReturn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorBaseValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorBaseValue(AThisValue).AsyncGeneratorReturn(
    AArgs, AThisValue);
end;

function TGocciaAsyncGeneratorPrototypeMethodHost.AsyncGeneratorThrow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAsyncGeneratorBaseValue) then
    Exit(RejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaAsyncGeneratorBaseValue(AThisValue).AsyncGeneratorThrow(
    AArgs, AThisValue);
end;

procedure EnsureGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
var
  Host: TGocciaGeneratorPrototypeMethodHost;
begin
  if not Assigned(APrototype) then
    Exit;

  Host := nil;
  if not APrototype.HasOwnProperty(PROP_NEXT) then
  begin
    if not Assigned(Host) then
      Host := GeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_NEXT,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.GeneratorNext, PROP_NEXT, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_RETURN) then
  begin
    if not Assigned(Host) then
      Host := GeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_RETURN,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.GeneratorReturn, PROP_RETURN, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_THROW) then
  begin
    if not Assigned(Host) then
      Host := GeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_THROW,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.GeneratorThrow, PROP_THROW, 1),
        [pfConfigurable, pfWritable]));
  end;
end;

procedure EnsureAsyncGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
var
  Host: TGocciaAsyncGeneratorPrototypeMethodHost;
begin
  if not Assigned(APrototype) then
    Exit;

  Host := nil;
  if not APrototype.HasOwnProperty(PROP_NEXT) then
  begin
    if not Assigned(Host) then
      Host := AsyncGeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_NEXT,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.AsyncGeneratorNext, PROP_NEXT, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_RETURN) then
  begin
    if not Assigned(Host) then
      Host := AsyncGeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_RETURN,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.AsyncGeneratorReturn, PROP_RETURN, 1),
        [pfConfigurable, pfWritable]));
  end;
  if not APrototype.HasOwnProperty(PROP_THROW) then
  begin
    if not Assigned(Host) then
      Host := AsyncGeneratorPrototypeMethodHost;
    APrototype.DefineProperty(PROP_THROW,
      TGocciaPropertyDescriptorData.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          Host.AsyncGeneratorThrow, PROP_THROW, 1),
        [pfConfigurable, pfWritable]));
  end;
end;

var
  GCreateGeneratorObject: TGocciaGeneratorObjectFactory;

procedure RegisterGeneratorObjectFactory(
  const AFactory: TGocciaGeneratorObjectFactory);
begin
  GCreateGeneratorObject := AFactory;
end;

function CreateGeneratorObject(const AFunction: TGocciaFunctionValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue;
  const AIsAsync: Boolean): TGocciaObjectValue;
begin
  if not Assigned(GCreateGeneratorObject) then
    raise Exception.Create(
      'A generator function was called, but no interpreter generator factory is registered');
  Result := GCreateGeneratorObject(AFunction, AArguments, AThisValue, AIsAsync);
end;

{ ES2026 §10.1.13 OrdinaryCreateFromConstructor(callee,
  "%GeneratorFunction.prototype.prototype%"): the generator object's prototype
  is the function's own `prototype` when that is an object, else the
  intrinsic. }
procedure SetGeneratorObjectPrototype(const AObject: TGocciaObjectValue;
  const AFunction: TGocciaFunctionValue;
  const AKind: TGocciaFunctionObjectIntrinsicKind);
var
  PrototypeValue: TGocciaValue;
begin
  PrototypeValue := AFunction.GetProperty(PROP_PROTOTYPE);
  if PrototypeValue is TGocciaObjectValue then
    AObject.Prototype := TGocciaObjectValue(PrototypeValue)
  else
    AObject.Prototype := GeneratorObjectIntrinsicParent(AKind,
      AFunction.CreationRealm);
end;

{ TGocciaGeneratorFunctionValue }

function TGocciaGeneratorFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  GeneratorObject: TGocciaObjectValue;
begin
  GeneratorObject := CreateGeneratorObject(Self, AArguments, AThisValue, False);
  SetGeneratorObjectPrototype(GeneratorObject, Self, foikGenerator);
  Result := GeneratorObject;
end;

function TGocciaGeneratorFunctionValue.IsConstructable: Boolean;
begin
  Result := False;
end;

{ TGocciaAsyncGeneratorFunctionValue }

function TGocciaAsyncGeneratorFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  AsyncGeneratorObject: TGocciaObjectValue;
begin
  AsyncGeneratorObject := CreateGeneratorObject(Self, AArguments, AThisValue,
    True);
  SetGeneratorObjectPrototype(AsyncGeneratorObject, Self, foikAsyncGenerator);
  Result := AsyncGeneratorObject;
end;

{ TGocciaGeneratorMethodValue }

function TGocciaGeneratorMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateGeneratorObject(Self, AArguments, AThisValue, False);
end;

{ TGocciaAsyncGeneratorMethodValue }

function TGocciaAsyncGeneratorMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateGeneratorObject(Self, AArguments, AThisValue, True);
end;

initialization
  GGeneratorPrototypeMethodHostSlot := RegisterRealmOwnedSlot(
    'Generator prototype method host');
  GAsyncGeneratorPrototypeMethodHostSlot := RegisterRealmOwnedSlot(
    'AsyncGenerator prototype method host');

end.
