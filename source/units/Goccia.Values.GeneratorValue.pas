// Generator machinery shared by the VM's generator objects: the generator
// object base classes and the next/return/throw methods on the generator
// prototypes. The generator objects themselves live in Goccia.VM.
unit Goccia.Values.GeneratorValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Realm,
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

procedure EnsureGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);
procedure EnsureAsyncGeneratorPrototypeMethods(const APrototype: TGocciaObjectValue);

function GeneratorIncompatibleReceiverError: TGocciaValue;
function RejectedTypeErrorPromise(const AMessage: string): TGocciaPromiseValue;

implementation

uses
  SysUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
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

initialization
  GGeneratorPrototypeMethodHostSlot := RegisterRealmOwnedSlot(
    'Generator prototype method host');
  GAsyncGeneratorPrototypeMethodHostSlot := RegisterRealmOwnedSlot(
    'AsyncGenerator prototype method host');

end.
