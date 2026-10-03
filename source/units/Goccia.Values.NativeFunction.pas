unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaNativeIntrinsicKind = (
    nikNone,
    nikDecodeURI,
    nikDecodeURIComponent,
    nikStringFromCharCode,
    { Function.prototype.bind. Marked so the bytecode VM's bound-function fast
      path can recognise the intrinsic itself rather than any callable that
      happens to be named `bind` — an own static named `bind` on a function
      object (node:async_hooks installs two) is a different function and must
      not be redirected into TGocciaBoundFunctionValue. }
    nikFunctionBind,
    { Function.prototype.call and Function.prototype.apply, marked for the same
      reason as bind: the VM's OP_CALL_METHOD fast paths invoke the receiver
      itself, so any other callable found under those names — `Reflect.apply`
      assigned as a function's own `apply`, a future built-in named `call` —
      must be called on its own terms instead. }
    nikFunctionCall,
    nikFunctionApply,
    { The getters of %TypedArray%.prototype.buffer, byteLength, byteOffset and
      length. Marked so a typed array's property read can tell that the
      accessor its prototype chain resolved to is still the built-in one, whose
      result it may then compute from the internal slots without a call. A
      getter a program installs under one of those names carries no kind and
      is called. }
    nikTypedArrayBuffer,
    nikTypedArrayByteLength,
    nikTypedArrayByteOffset,
    nikTypedArrayLength,
    { The getters of ArrayBuffer.prototype.byteLength, maxByteLength,
      resizable, detached and immutable, and of SharedArrayBuffer.prototype
      .byteLength, maxByteLength and growable, marked for the same reason. }
    nikArrayBufferByteLength,
    nikArrayBufferMaxByteLength,
    nikArrayBufferResizable,
    nikArrayBufferDetached,
    nikArrayBufferImmutable,
    nikSharedArrayBufferByteLength,
    nikSharedArrayBufferMaxByteLength,
    nikSharedArrayBufferGrowable
  );

  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunctionCallback;
    FConstructCallback: TGocciaNativeConstructorCallback;
    FCachedIntrinsicProto: TGocciaObjectValue;
    FName: string;
    FArity: Integer;
    FNotConstructable: Boolean;
    FDirectEvalHost: Boolean;
    FCapturedRoot: TGocciaValue;
    FIntrinsicKind: TGocciaNativeIntrinsicKind;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AFunction: TGocciaNativeFunctionCallback; const AName: string;
      const AArity: Integer);
    constructor CreateWithoutPrototype(const AFunction: TGocciaNativeFunctionCallback; const AName: string;
      const AArity: Integer);
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function Construct(const AArguments: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function IsConstructable: Boolean; override;
    procedure MarkReferences; override;
    property NativeFunction: TGocciaNativeFunctionCallback read FFunction;
    property ConstructCallback: TGocciaNativeConstructorCallback read FConstructCallback write FConstructCallback;
    property Name: string read FName;
    property Arity: Integer read FArity;
    property NotConstructable: Boolean read FNotConstructable write FNotConstructable;
    property DirectEvalHost: Boolean read FDirectEvalHost write FDirectEvalHost;
    property CapturedRoot: TGocciaValue read FCapturedRoot write FCapturedRoot;
    property IntrinsicKind: TGocciaNativeIntrinsicKind
      read FIntrinsicKind write FIntrinsicKind;
  end;

{ Stamps AKind on the native getter of APrototype's own accessor AName, so
  ResolvePropertyWithoutCall recognises the built-in getter itself rather than
  whatever function a program later defines under the same name. }
procedure MarkIntrinsicGetter(const APrototype: TGocciaObjectValue;
  const AName: string; const AKind: TGocciaNativeIntrinsicKind);

{ The ordinary lookup of AName from AObject (ES2026 §10.1.8.1 OrdinaryGet), for
  the chains it can answer without calling anything: AObject's own property
  map, then each prototype while that prototype is a plain object. AObject must
  keep all its own properties in that map. Returns True with AKind = nikNone
  and AValue set for a plain data property, or to undefined when the chain ends
  without the property. Returns True with AValue nil and AKind set for an
  accessor whose getter is a native function marked by MarkIntrinsicGetter; the
  caller computes that getter's result for AObject itself as the receiver, or
  declines if the kind is not one of its own. Returns False for anything else
  (a getter a program defined, a lazy property, a prototype that is not a plain
  object), which leaves the read to the full lookup. No managed locals: buffers
  read every named property through it. }
function ResolvePropertyWithoutCall(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: TGocciaValue;
  out AKind: TGocciaNativeIntrinsicKind): Boolean;


implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Realm,
  Goccia.Values.ObjectPropertyDescriptor;

procedure MarkIntrinsicGetter(const APrototype: TGocciaObjectValue;
  const AName: string; const AKind: TGocciaNativeIntrinsicKind);
var
  Descriptor: TGocciaPropertyDescriptor;
  Getter: TGocciaValue;
begin
  Descriptor := APrototype.GetOwnPropertyDescriptor(AName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
    Getter := TGocciaPropertyDescriptorAccessor(Descriptor).Getter
  else
    Getter := nil;
  // A miss here would silently send every read of AName through the getter
  // call: still correct, so no behaviour test could notice.
  Assert(Getter is TGocciaNativeFunctionValue,
    AName + ' must have a native getter to carry its intrinsic kind');
  // Production builds compile assertions out (source/shared/Shared.inc), so
  // the type test has to stand on its own before the cast.
  if Getter is TGocciaNativeFunctionValue then
    TGocciaNativeFunctionValue(Getter).IntrinsicKind := AKind;
end;

function ResolvePropertyWithoutCall(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: TGocciaValue;
  out AKind: TGocciaNativeIntrinsicKind): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
  Getter: TGocciaValue;
  Holder: TGocciaObjectValue;
begin
  AValue := nil;
  AKind := nikNone;
  Result := False;
  Holder := AObject;
  repeat
    if Holder.Properties.TryGetValue(AName, Descriptor) then
    begin
      // Exact classes: a not-yet-materialized lazy descriptor is a subclass of
      // the data descriptor and must take the full lookup, which replaces it.
      if Descriptor.ClassType = TGocciaPropertyDescriptorData then
      begin
        AValue := TGocciaPropertyDescriptorData(Descriptor).Value;
        Exit(True);
      end;
      if Descriptor.ClassType <> TGocciaPropertyDescriptorAccessor then
        Exit;
      Getter := TGocciaPropertyDescriptorAccessor(Descriptor).Getter;
      if (not Assigned(Getter)) or
         (Getter.ClassType <> TGocciaNativeFunctionValue) then
        Exit;
      AKind := TGocciaNativeFunctionValue(Getter).IntrinsicKind;
      Result := AKind <> nikNone;
      Exit;
    end;
    Holder := Holder.Prototype;
  until (not Assigned(Holder)) or (Holder.ClassType <> TGocciaObjectValue);
  if not Assigned(Holder) then
  begin
    AValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result := True;
  end;
end;

constructor TGocciaNativeFunctionValue.Create(const AFunction: TGocciaNativeFunctionCallback;
  const AName: string; const AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create;
end;

constructor TGocciaNativeFunctionValue.CreateWithoutPrototype(const AFunction: TGocciaNativeFunctionCallback;
  const AName: string; const AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;
  FNotConstructable := True;

  inherited Create; // No prototype for methods that are part of the prototype
end;

function TGocciaNativeFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  PreviousRealm: TGocciaRealm;
  RealmSwitched: Boolean;
begin
  PreviousRealm := CurrentRealm;
  RealmSwitched := Assigned(FCreationRealm) and
    (FCreationRealm <> PreviousRealm);
  // A same-realm call, which is nearly every call, has nothing to restore and
  // so needs no exception frame.
  if not RealmSwitched then
    Exit(FFunction(AArguments, AThisValue));
  SetCurrentRealm(FCreationRealm);
  try
    Result := FFunction(AArguments, AThisValue);
  finally
    SetCurrentRealm(PreviousRealm);
  end;
end;

function TGocciaNativeFunctionValue.Construct(const AArguments: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
var
  PreviousRealm: TGocciaRealm;
  ProtoValue: TGocciaValue;
  RealmSwitched: Boolean;
begin
  PreviousRealm := CurrentRealm;
  RealmSwitched := Assigned(FCreationRealm) and
    (FCreationRealm <> PreviousRealm);
  if RealmSwitched then
    SetCurrentRealm(FCreationRealm);
  try
    if Assigned(FConstructCallback) then
      Result := FConstructCallback(AArguments, ANewTarget)
    else
    begin
      Result := FFunction(AArguments, TGocciaHoleValue.HoleValue);
      // Error / Promise register explicit ConstructCallbacks because their
      // specs require prototype resolution before argument coercion.
      if (TGocciaValue(ANewTarget) <> TGocciaValue(Self)) and
         (Result is TGocciaObjectValue) then
      begin
        if not Assigned(FCachedIntrinsicProto) then
        begin
          ProtoValue := Self.GetProperty(PROP_PROTOTYPE);
          if ProtoValue is TGocciaObjectValue then
            FCachedIntrinsicProto := TGocciaObjectValue(ProtoValue)
          else
            FCachedIntrinsicProto := TGocciaObjectValue.SharedObjectPrototype;
        end;
        TGocciaObjectValue(Result).Prototype :=
          GetProtoFromConstructorWithIntrinsic(ANewTarget, FCachedIntrinsicProto);
      end;
    end;
  finally
    if RealmSwitched then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function TGocciaNativeFunctionValue.IsConstructable: Boolean;
begin
  Result := not FNotConstructable;
end;

procedure TGocciaNativeFunctionValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCapturedRoot) then
    FCapturedRoot.MarkReferences;
end;

function TGocciaNativeFunctionValue.GetFunctionLength: Integer;
begin
  // -1 means variadic, report 0 for length per ECMAScript spec
  if FArity < 0 then
    Result := 0
  else
    Result := FArity;
end;

function TGocciaNativeFunctionValue.GetFunctionName: string;
begin
  Result := FName;
end;

end.
