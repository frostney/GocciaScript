unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Realm,
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
    // Heap snapshot prototype: AWriter and AReader are the snapshot writer
    // and reader.
    procedure SnapshotWriteState(const AWriter: TObject);
    procedure SnapshotReadState(const AReader: TObject);
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


{ Records AFunction as ARealm's %eval% (ES2026 §19.2.1) when a host installs
  the global `eval`, and marks the realm as hosting direct eval. The realm
  keeps it alive, so the identity check below holds after a script replaces
  or deletes the global property. }
procedure InstallRealmEvalFunction(const ARealm: TGocciaRealm;
  const AFunction: TGocciaNativeFunctionValue);

{ ES2026 §13.3.6.1 step 6.a: SameValue(func, %eval%) for ARealm. Compares
  with the function the host installed, not the current global `eval`. }
function IsRealmEvalFunction(const ARealm: TGocciaRealm;
  const AValue: TGocciaValue): Boolean;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot.Codec,
  Goccia.Values.ObjectPropertyDescriptor;

var
  GEvalFunctionSlot: TGocciaRealmSlotId;

procedure InstallRealmEvalFunction(const ARealm: TGocciaRealm;
  const AFunction: TGocciaNativeFunctionValue);
begin
  AFunction.DirectEvalHost := True;
  ARealm.SetSlot(GEvalFunctionSlot, AFunction);
end;

function IsRealmEvalFunction(const ARealm: TGocciaRealm;
  const AValue: TGocciaValue): Boolean;
begin
  Result := Assigned(ARealm) and Assigned(AValue) and
    (ARealm.GetSlot(GEvalFunctionSlot) = AValue);
end;

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
  ReleaseDescriptor(Descriptor);
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

{ Heap snapshot prototype }

type
  TNativeFunctionSnapshotCodec = class(TGocciaSnapshotCodec)
  public
    function Tag: string; override;
    procedure Write(const AWriter: TGocciaHeapSnapshotWriter;
      const AObject: TGCManagedObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader):
      TGCManagedObject; override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const AObject: TGCManagedObject); override;
  end;

procedure TGocciaNativeFunctionValue.SnapshotWriteState(const AWriter: TObject);
var
  W: TGocciaHeapSnapshotWriter;
  CallbackName: string;
begin
  W := TGocciaHeapSnapshotWriter(AWriter);
  if Assigned(TMethod(FConstructCallback).Code) then
    W.Refuse('native function "' + FName + '" with a construct callback');
  if FDirectEvalHost then
    W.Refuse('the direct-eval host function');
  CallbackName := SnapshotNativeCallbackName(TMethod(FFunction).Code);
  if CallbackName = '' then
    W.Refuse('native function "' + FName + '" with an unregistered callback');
  if TMethod(FFunction).Data <> Pointer(FCapturedRoot) then
    W.Refuse('native function "' + FName +
      '" bound to something other than its captured root');
  W.WriteStr(CallbackName);
  W.WriteValue(FCapturedRoot);
  W.WriteStr(FName);
  W.WriteI32(FArity);
  W.WriteBool(FNotConstructable);
  W.WriteU8(Ord(FIntrinsicKind));
  W.WriteBool(FStrictThis);
  W.WriteBool(FStrictCode);
  W.WriteBool(FHasOwnLengthProperty);
  W.WriteBool(FHasOwnNameProperty);
  W.WriteObjectCore(Self);
end;

procedure TGocciaNativeFunctionValue.SnapshotReadState(const AReader: TObject);
var
  R: TGocciaHeapSnapshotReader;
  Code: Pointer;
  Kind: Byte;
begin
  R := TGocciaHeapSnapshotReader(AReader);
  Code := SnapshotNativeCallbackCode(R.ReadStr);
  if not Assigned(Code) then
    R.Refuse('unknown native callback');
  FCapturedRoot := R.ReadValue;
  if not Assigned(FCapturedRoot) then
    R.Refuse('native function without its captured root');
  TMethod(FFunction).Code := Code;
  TMethod(FFunction).Data := Pointer(FCapturedRoot);
  FName := R.ReadStr;
  FArity := R.ReadI32;
  FNotConstructable := R.ReadBool;
  Kind := R.ReadU8;
  if Kind > Ord(High(TGocciaNativeIntrinsicKind)) then
    R.Refuse('bad intrinsic kind');
  FIntrinsicKind := TGocciaNativeIntrinsicKind(Kind);
  FStrictThis := R.ReadBool;
  FStrictCode := R.ReadBool;
  FHasOwnLengthProperty := R.ReadBool;
  FHasOwnNameProperty := R.ReadBool;
  FCreationRealm := CurrentRealm;
  R.ReadObjectCore(Self);
end;

function TNativeFunctionSnapshotCodec.Tag: string;
begin
  Result := 'native-function';
end;

procedure TNativeFunctionSnapshotCodec.Write(
  const AWriter: TGocciaHeapSnapshotWriter; const AObject: TGCManagedObject);
begin
  TGocciaNativeFunctionValue(AObject).SnapshotWriteState(AWriter);
end;

function TNativeFunctionSnapshotCodec.Allocate(
  const AReader: TGocciaHeapSnapshotReader): TGCManagedObject;
begin
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(nil, '', 0);
end;

procedure TNativeFunctionSnapshotCodec.Fill(
  const AReader: TGocciaHeapSnapshotReader; const AObject: TGCManagedObject);
begin
  TGocciaNativeFunctionValue(AObject).SnapshotReadState(AReader);
end;

initialization
  RegisterSnapshotCodec(TGocciaNativeFunctionValue,
    TNativeFunctionSnapshotCodec.Create);
  GEvalFunctionSlot := RegisterRealmSlot('eval');

end.
