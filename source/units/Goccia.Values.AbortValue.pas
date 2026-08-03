unit Goccia.Values.AbortValue;

// WHATWG DOM AbortController and AbortSignal state values.

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaAbortSignalValue = class(TGocciaInstanceValue)
  private
    FReason: TGocciaValue;
    FTimeoutDeadlineNanoseconds: Int64;
    function AbortedGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ReasonGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ThrowIfAborted(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    function IsAborted: Boolean;
    function RemainingTimeoutMilliseconds: Integer;
    procedure RefreshTimeout;
    procedure SetTimeout(const AMilliseconds: Double);
    procedure SignalAbort(const AReason: TGocciaValue = nil);
    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Reason: TGocciaValue read FReason;
  end;

  TGocciaAbortControllerValue = class(TGocciaInstanceValue)
  private
    FSignal: TGocciaAbortSignalValue;
    function Abort(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SignalGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Signal: TGocciaAbortSignalValue read FSignal;
  end;

implementation

uses
  Math,
  SysUtils,

  TimingUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor;

const
  NANOSECONDS_PER_MILLISECOND = 1000000;
  ABORT_ERROR_MESSAGE = 'This operation was aborted';
  TIMEOUT_ERROR_MESSAGE = 'The operation was aborted due to timeout';

var
  GAbortControllerSharedSlot: TGocciaRealmOwnedSlotId;
  GAbortSignalSharedSlot: TGocciaRealmOwnedSlotId;

function GetAbortControllerShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if CurrentRealm <> nil then
    Result := TGocciaSharedPrototype(
      CurrentRealm.GetOwnedSlot(GAbortControllerSharedSlot))
  else
    Result := nil;
end;

function GetAbortSignalShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if CurrentRealm <> nil then
    Result := TGocciaSharedPrototype(
      CurrentRealm.GetOwnedSlot(GAbortSignalSharedSlot))
  else
    Result := nil;
end;

{ TGocciaAbortSignalValue }

constructor TGocciaAbortSignalValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FReason := TGocciaUndefinedLiteralValue.UndefinedValue;
  FTimeoutDeadlineNanoseconds := 0;
  InitializePrototype;
  Shared := GetAbortSignalShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaAbortSignalValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if CurrentRealm = nil then
    Exit;
  if GetAbortSignalShared <> nil then
    Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GAbortSignalSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor(PROP_ABORTED, AbortedGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_REASON, ReasonGetter, nil, [pfConfigurable]);
    Members.AddNamedMethod(PROP_THROW_IF_ABORTED, ThrowIfAborted, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaAbortSignalValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
  Bootstrap: TGocciaAbortSignalValue;
begin
  Shared := GetAbortSignalShared;
  if not Assigned(Shared) then
  begin
    Bootstrap := TGocciaAbortSignalValue.Create;
    Shared := GetAbortSignalShared;
    if not Assigned(Shared) then
      Bootstrap.Free;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

// WHATWG DOM §3.2: an AbortSignal is aborted when its abort reason is not undefined.
function TGocciaAbortSignalValue.IsAborted: Boolean;
begin
  RefreshTimeout;
  Result := not (FReason is TGocciaUndefinedLiteralValue);
end;

// WHATWG DOM §3.2 AbortSignal.timeout(milliseconds), observed at a host checkpoint.
procedure TGocciaAbortSignalValue.RefreshTimeout;
begin
  if (FTimeoutDeadlineNanoseconds > 0) and
     (FReason is TGocciaUndefinedLiteralValue) and
     (GetNanoseconds >= FTimeoutDeadlineNanoseconds) then
    FReason := CreateDOMExceptionObject(TIMEOUT_ERROR_NAME,
      TIMEOUT_ERROR_MESSAGE);
end;

procedure TGocciaAbortSignalValue.SetTimeout(const AMilliseconds: Double);
var
  NowNanoseconds, MaximumMilliseconds: Int64;
begin
  NowNanoseconds := GetNanoseconds;
  MaximumMilliseconds :=
    (High(Int64) - NowNanoseconds) div NANOSECONDS_PER_MILLISECOND;
  if AMilliseconds >= MaximumMilliseconds then
    FTimeoutDeadlineNanoseconds := High(Int64)
  else
    FTimeoutDeadlineNanoseconds := NowNanoseconds +
      Trunc(AMilliseconds) * NANOSECONDS_PER_MILLISECOND;
end;

function TGocciaAbortSignalValue.RemainingTimeoutMilliseconds: Integer;
var
  RemainingNanoseconds: Int64;
begin
  RefreshTimeout;
  if FTimeoutDeadlineNanoseconds = 0 then
    Exit(-1);
  if not (FReason is TGocciaUndefinedLiteralValue) then
    Exit(0);

  RemainingNanoseconds := FTimeoutDeadlineNanoseconds - GetNanoseconds;
  if RemainingNanoseconds <= 0 then
    Exit(0);
  RemainingNanoseconds :=
    (RemainingNanoseconds + NANOSECONDS_PER_MILLISECOND - 1) div
    NANOSECONDS_PER_MILLISECOND;
  if RemainingNanoseconds > High(Integer) then
    Result := High(Integer)
  else
    Result := Integer(RemainingNanoseconds);
end;

// WHATWG DOM §3.2 signal abort.
procedure TGocciaAbortSignalValue.SignalAbort(const AReason: TGocciaValue);
begin
  RefreshTimeout;
  if not (FReason is TGocciaUndefinedLiteralValue) then
    Exit;

  if not Assigned(AReason) or
     (AReason is TGocciaUndefinedLiteralValue) then
    FReason := CreateDOMExceptionObject(ABORT_ERROR_NAME,
      ABORT_ERROR_MESSAGE)
  else
    FReason := AReason;
end;

// WHATWG DOM §3.2 get AbortSignal.prototype.aborted.
function TGocciaAbortSignalValue.AbortedGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAbortSignalValue) then
    ThrowTypeError('AbortSignal.prototype.aborted called on incompatible receiver');
  if TGocciaAbortSignalValue(AThisValue).IsAborted then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG DOM §3.2 get AbortSignal.prototype.reason.
function TGocciaAbortSignalValue.ReasonGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Signal: TGocciaAbortSignalValue;
begin
  if not (AThisValue is TGocciaAbortSignalValue) then
    ThrowTypeError('AbortSignal.prototype.reason called on incompatible receiver');
  Signal := TGocciaAbortSignalValue(AThisValue);
  Signal.RefreshTimeout;
  Result := Signal.Reason;
end;

// WHATWG DOM §3.2 AbortSignal.prototype.throwIfAborted().
function TGocciaAbortSignalValue.ThrowIfAborted(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Signal: TGocciaAbortSignalValue;
begin
  if not (AThisValue is TGocciaAbortSignalValue) then
    ThrowTypeError(
      'AbortSignal.prototype.throwIfAborted called on incompatible receiver');
  Signal := TGocciaAbortSignalValue(AThisValue);
  if Signal.IsAborted then
    raise TGocciaThrowValue.Create(Signal.Reason);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaAbortSignalValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_ABORT_SIGNAL;
end;

procedure TGocciaAbortSignalValue.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FReason) then
    FReason.MarkReferences;
end;

{ TGocciaAbortControllerValue }

constructor TGocciaAbortControllerValue.Create(
  const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FSignal := TGocciaAbortSignalValue.Create;
  InitializePrototype;
  Shared := GetAbortControllerShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaAbortControllerValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if CurrentRealm = nil then
    Exit;
  if GetAbortControllerShared <> nil then
    Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GAbortControllerSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor(PROP_SIGNAL, SignalGetter, nil, [pfConfigurable]);
    Members.AddNamedMethod(PROP_ABORT, Abort, 1, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaAbortControllerValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
  Bootstrap: TGocciaAbortControllerValue;
begin
  Shared := GetAbortControllerShared;
  if not Assigned(Shared) then
  begin
    Bootstrap := TGocciaAbortControllerValue.Create;
    Shared := GetAbortControllerShared;
    if not Assigned(Shared) then
      Bootstrap.Free;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

// WHATWG DOM §3.1 AbortController.prototype.abort(reason).
function TGocciaAbortControllerValue.Abort(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Controller: TGocciaAbortControllerValue;
begin
  if not (AThisValue is TGocciaAbortControllerValue) then
    ThrowTypeError(
      'AbortController.prototype.abort called on incompatible receiver');
  Controller := TGocciaAbortControllerValue(AThisValue);
  if AArgs.Length > 0 then
    Controller.Signal.SignalAbort(AArgs.GetElement(0))
  else
    Controller.Signal.SignalAbort;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// WHATWG DOM §3.1 get AbortController.prototype.signal.
function TGocciaAbortControllerValue.SignalGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaAbortControllerValue) then
    ThrowTypeError(
      'AbortController.prototype.signal called on incompatible receiver');
  Result := TGocciaAbortControllerValue(AThisValue).Signal;
end;

function TGocciaAbortControllerValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_ABORT_CONTROLLER;
end;

procedure TGocciaAbortControllerValue.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FSignal) then
    FSignal.MarkReferences;
end;

initialization
  GAbortControllerSharedSlot :=
    RegisterRealmOwnedSlot('AbortController.shared');
  GAbortSignalSharedSlot := RegisterRealmOwnedSlot('AbortSignal.shared');

end.
