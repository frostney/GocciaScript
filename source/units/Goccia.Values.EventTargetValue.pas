unit Goccia.Values.EventTargetValue;

// WHATWG DOM EventTarget (https://dom.spec.whatwg.org/#interface-eventtarget).
//
// GocciaScript has no node tree, so dispatch is always a single-target
// invocation: the event listener list of one object, in registration order.
// `capture` is accepted and participates in listener identity (per WHATWG DOM
// §2.7, a listener is keyed on (type, callback, capture)) but has no ordering
// effect, and `passive` is accepted and ignored. The `signal` member of
// AddEventListenerOptions is not supported. See
// docs/adr/0104-whatwg-eventtarget-base.md.

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.EventValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // WHATWG DOM §2.7 event listener: a struct, not a JavaScript value. The
  // callback is a GC-managed value and is marked through the owning target.
  TGocciaEventListener = class
  private
    FEventType: string;
    FCallback: TGocciaValue;
    FCapture: Boolean;
    FOnce: Boolean;
    FRemoved: Boolean;
    FEventHandler: Boolean;
  public
    constructor Create(const AEventType: string; const ACallback: TGocciaValue;
      const ACapture, AOnce, AEventHandler: Boolean);

    property EventType: string read FEventType;
    property Callback: TGocciaValue read FCallback write FCallback;
    property Capture: Boolean read FCapture;
    property Once: Boolean read FOnce;
    property Removed: Boolean read FRemoved write FRemoved;
    property EventHandler: Boolean read FEventHandler;
  end;

  TGocciaEventListenerList = TObjectList<TGocciaEventListener>;

  TGocciaEventTargetValue = class(TGocciaInstanceValue)
  private
    FListeners: TGocciaEventListenerList;
    FDispatchDepth: Integer;
    FHasRemovedListeners: Boolean;
    function AddEventListenerMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RemoveEventListenerMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DispatchEventMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InitializeEventTargetPrototype;
    procedure CompactListeners;
    function FindListener(const AEventType: string;
      const ACallback: TGocciaValue;
      const ACapture: Boolean): TGocciaEventListener;
    function FindEventHandlerListener(
      const AEventType: string): TGocciaEventListener;
    procedure InvokeListener(const AListener: TGocciaEventListener;
      const AEvent: TGocciaEventValue);
  protected
    // WHATWG DOM §8.1.5.1 event handler IDL attributes, used by subclasses that
    // expose an `on<type>` accessor (AbortSignal.onabort).
    function EventHandlerValue(const AEventType: string): TGocciaValue;
    procedure SetEventHandlerValue(const AEventType: string;
      const AValue: TGocciaValue);
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    procedure AddListener(const AEventType: string;
      const ACallback: TGocciaValue; const ACapture, AOnce: Boolean);
    procedure RemoveListener(const AEventType: string;
      const ACallback: TGocciaValue; const ACapture: Boolean);
    function DispatchEventValue(const AEvent: TGocciaEventValue): Boolean;

    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class function SharedPrototypeObject: TGocciaObjectValue;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Utils,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper;

const
  ALREADY_DISPATCHING_MESSAGE =
    'The event is already being dispatched';

var
  GEventTargetSharedSlot: TGocciaRealmOwnedSlotId;

function GetEventTargetShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if CurrentRealm <> nil then
    Result := TGocciaSharedPrototype(
      CurrentRealm.GetOwnedSlot(GEventTargetSharedSlot))
  else
    Result := nil;
end;

// WHATWG DOM §2.7 flatten / flatten more: an options argument that is a
// boolean is the capture flag; an object supplies capture/once/passive.
function FlattenBooleanOption(const AOptions: TGocciaValue;
  const AName: string): Boolean;
var
  Member: TGocciaValue;
begin
  if not Assigned(AOptions) then
    Exit(False);
  if AOptions is TGocciaObjectValue then
  begin
    Member := TGocciaObjectValue(AOptions).GetProperty(AName);
    Result := Assigned(Member) and Member.ToBooleanLiteral.Value;
  end
  else if AName = PROP_CAPTURE then
    Result := AOptions.ToBooleanLiteral.Value
  else
    Result := False;
end;

{ TGocciaEventListener }

constructor TGocciaEventListener.Create(const AEventType: string;
  const ACallback: TGocciaValue;
  const ACapture, AOnce, AEventHandler: Boolean);
begin
  inherited Create;
  FEventType := AEventType;
  FCallback := ACallback;
  FCapture := ACapture;
  FOnce := AOnce;
  FRemoved := False;
  FEventHandler := AEventHandler;
end;

{ TGocciaEventTargetValue }

constructor TGocciaEventTargetValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FListeners := TGocciaEventListenerList.Create(True);
  FDispatchDepth := 0;
  FHasRemovedListeners := False;
  InitializeEventTargetPrototype;
  Shared := GetEventTargetShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaEventTargetValue.Destroy;
begin
  FListeners.Free;
  inherited;
end;

procedure TGocciaEventTargetValue.InitializeEventTargetPrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if CurrentRealm = nil then
    Exit;
  if GetEventTargetShared <> nil then
    Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GEventTargetSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_ADD_EVENT_LISTENER, AddEventListenerMethod, 2,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_REMOVE_EVENT_LISTENER,
      RemoveEventListenerMethod, 2, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_DISPATCH_EVENT, DispatchEventMethod, 1,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaEventTargetValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
  Bootstrap: TGocciaEventTargetValue;
begin
  Shared := GetEventTargetShared;
  if not Assigned(Shared) then
  begin
    Bootstrap := TGocciaEventTargetValue.Create;
    Shared := GetEventTargetShared;
    if not Assigned(Shared) then
      Bootstrap.Free;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

class function TGocciaEventTargetValue.SharedPrototypeObject: TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetEventTargetShared;
  if Assigned(Shared) then
    Result := Shared.Prototype
  else
    Result := nil;
end;

function TGocciaEventTargetValue.FindListener(const AEventType: string;
  const ACallback: TGocciaValue;
  const ACapture: Boolean): TGocciaEventListener;
var
  I: Integer;
  Candidate: TGocciaEventListener;
begin
  Result := nil;
  for I := 0 to FListeners.Count - 1 do
  begin
    Candidate := FListeners[I];
    // Event handler listeners have an internal callback and never take part in
    // addEventListener/removeEventListener identity matching.
    if Candidate.EventHandler or Candidate.Removed then
      Continue;
    if (Candidate.EventType = AEventType) and
       (Candidate.Callback = ACallback) and
       (Candidate.Capture = ACapture) then
      Exit(Candidate);
  end;
end;

function TGocciaEventTargetValue.FindEventHandlerListener(
  const AEventType: string): TGocciaEventListener;
var
  I: Integer;
  Candidate: TGocciaEventListener;
begin
  Result := nil;
  for I := 0 to FListeners.Count - 1 do
  begin
    Candidate := FListeners[I];
    if Candidate.EventHandler and (Candidate.EventType = AEventType) then
      Exit(Candidate);
  end;
end;

procedure TGocciaEventTargetValue.CompactListeners;
var
  I: Integer;
begin
  if not FHasRemovedListeners then
    Exit;
  for I := FListeners.Count - 1 downto 0 do
    if FListeners[I].Removed then
      FListeners.Delete(I);
  FHasRemovedListeners := False;
end;

// WHATWG DOM §2.7 add an event listener.
procedure TGocciaEventTargetValue.AddListener(const AEventType: string;
  const ACallback: TGocciaValue; const ACapture, AOnce: Boolean);
begin
  // Step 3: if an equivalent listener is already present, do not add again.
  if Assigned(FindListener(AEventType, ACallback, ACapture)) then
    Exit;
  FListeners.Add(TGocciaEventListener.Create(AEventType, ACallback, ACapture,
    AOnce, False));
end;

// WHATWG DOM §2.7 remove an event listener.
procedure TGocciaEventTargetValue.RemoveListener(const AEventType: string;
  const ACallback: TGocciaValue; const ACapture: Boolean);
var
  Listener: TGocciaEventListener;
begin
  Listener := FindListener(AEventType, ACallback, ACapture);
  if not Assigned(Listener) then
    Exit;
  // Set removed first so an in-flight dispatch skips it, then compact when no
  // dispatch is walking the list.
  Listener.Removed := True;
  FHasRemovedListeners := True;
  if FDispatchDepth = 0 then
    CompactListeners;
end;

// WHATWG DOM §8.1.5.1: the event handler's listener is registered the first
// time a non-null handler is assigned, and setting null afterwards only clears
// the handler value — the listener keeps its registration order.
function TGocciaEventTargetValue.EventHandlerValue(
  const AEventType: string): TGocciaValue;
var
  Listener: TGocciaEventListener;
begin
  Listener := FindEventHandlerListener(AEventType);
  if Assigned(Listener) and Assigned(Listener.Callback) then
    Result := Listener.Callback
  else
    Result := TGocciaNullLiteralValue.NullValue;
end;

procedure TGocciaEventTargetValue.SetEventHandlerValue(
  const AEventType: string; const AValue: TGocciaValue);
var
  Listener: TGocciaEventListener;
  Handler: TGocciaValue;
begin
  if Assigned(AValue) and AValue.IsCallable then
    Handler := AValue
  else
    Handler := TGocciaNullLiteralValue.NullValue;

  Listener := FindEventHandlerListener(AEventType);
  if Assigned(Listener) then
  begin
    Listener.Callback := Handler;
    Exit;
  end;

  if Handler is TGocciaNullLiteralValue then
    Exit;
  FListeners.Add(TGocciaEventListener.Create(AEventType, Handler, False, False,
    True));
end;

procedure TGocciaEventTargetValue.InvokeListener(
  const AListener: TGocciaEventListener; const AEvent: TGocciaEventValue);
var
  Callback, Handler, Receiver: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  Callback := AListener.Callback;
  if not Assigned(Callback) or (Callback is TGocciaNullLiteralValue) or
     (Callback is TGocciaUndefinedLiteralValue) then
    Exit;

  Handler := Callback;
  Receiver := Self;
  // WHATWG DOM §2.9 inner invoke: a non-callable object listener is invoked
  // through its `handleEvent` method, with the listener object as `this`.
  if not Callback.IsCallable then
  begin
    if not (Callback is TGocciaObjectValue) then
      Exit;
    Handler := TGocciaObjectValue(Callback).GetProperty(PROP_HANDLE_EVENT);
    if not Assigned(Handler) or not Handler.IsCallable then
      Exit;
    Receiver := Callback;
  end;

  CallArgs := TGocciaArgumentsCollection.Create([AEvent]);
  try
    InvokeCallable(Handler, CallArgs, Receiver);
  finally
    CallArgs.Free;
  end;
end;

// WHATWG DOM §2.9 dispatch, reduced to a single target: no propagation path is
// built, so the event's listener list is invoked in registration order.
function TGocciaEventTargetValue.DispatchEventValue(
  const AEvent: TGocciaEventValue): Boolean;
var
  I, InitialCount: Integer;
  Listener: TGocciaEventListener;
  Rooted: Boolean;
begin
  Rooted := TGarbageCollector.Instance <> nil;
  if Rooted then
    TGarbageCollector.Instance.AddTempRoot(AEvent);
  try
    AEvent.DispatchFlag := True;
    AEvent.Target := Self;
    AEvent.CurrentTarget := Self;
    Inc(FDispatchDepth);
    try
      // Listeners appended while dispatching are not invoked for this event.
      InitialCount := FListeners.Count;
      for I := 0 to InitialCount - 1 do
      begin
        if I >= FListeners.Count then
          Break;
        Listener := FListeners[I];
        if Listener.Removed or (Listener.EventType <> AEvent.EventType) then
          Continue;
        // WHATWG DOM §2.9: a `once` listener is removed before it is invoked.
        if Listener.Once then
        begin
          Listener.Removed := True;
          FHasRemovedListeners := True;
        end;
        InvokeListener(Listener, AEvent);
      end;
    finally
      // Unwind the dispatch state even when a listener throws, so the event
      // stays reusable and the listener list is still compacted.
      Dec(FDispatchDepth);
      if FDispatchDepth = 0 then
        CompactListeners;
      AEvent.CurrentTarget := TGocciaNullLiteralValue.NullValue;
      AEvent.DispatchFlag := False;
    end;
    Result := not (AEvent.Cancelable and AEvent.DefaultPrevented);
  finally
    if Rooted and (TGarbageCollector.Instance <> nil) then
      TGarbageCollector.Instance.RemoveTempRoot(AEvent);
  end;
end;

// WHATWG DOM §2.7 EventTarget.prototype.addEventListener(type, callback,
// options).
function TGocciaEventTargetValue.AddEventListenerMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaEventTargetValue;
  Callback, Options: TGocciaValue;
  EventType: string;
begin
  if not (AThisValue is TGocciaEventTargetValue) then
    ThrowTypeError(
      'EventTarget.prototype.addEventListener called on incompatible receiver');
  Target := TGocciaEventTargetValue(AThisValue);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then
    ThrowTypeError(
      'EventTarget.prototype.addEventListener requires a type and a callback');
  EventType := AArgs.GetElement(0).ToStringLiteral.Value;
  Callback := AArgs.GetElement(1);
  // WebIDL EventListener?: null and undefined are a no-op, other non-objects
  // are a TypeError.
  if (Callback is TGocciaNullLiteralValue) or
     (Callback is TGocciaUndefinedLiteralValue) then
    Exit;
  if not (Callback is TGocciaObjectValue) then
    ThrowTypeError(
      'EventTarget.prototype.addEventListener callback must be an object or null');

  if AArgs.Length > 2 then
    Options := AArgs.GetElement(2)
  else
    Options := nil;

  Target.AddListener(EventType, Callback,
    FlattenBooleanOption(Options, PROP_CAPTURE),
    FlattenBooleanOption(Options, PROP_ONCE));
end;

// WHATWG DOM §2.7 EventTarget.prototype.removeEventListener(type, callback,
// options).
function TGocciaEventTargetValue.RemoveEventListenerMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaEventTargetValue;
  Callback, Options: TGocciaValue;
  EventType: string;
begin
  if not (AThisValue is TGocciaEventTargetValue) then
    ThrowTypeError(
      'EventTarget.prototype.removeEventListener called on incompatible receiver');
  Target := TGocciaEventTargetValue(AThisValue);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;

  if AArgs.Length < 2 then
    ThrowTypeError(
      'EventTarget.prototype.removeEventListener requires a type and a callback');
  EventType := AArgs.GetElement(0).ToStringLiteral.Value;
  Callback := AArgs.GetElement(1);
  if (Callback is TGocciaNullLiteralValue) or
     (Callback is TGocciaUndefinedLiteralValue) then
    Exit;
  if not (Callback is TGocciaObjectValue) then
    ThrowTypeError(
      'EventTarget.prototype.removeEventListener callback must be an object or null');

  if AArgs.Length > 2 then
    Options := AArgs.GetElement(2)
  else
    Options := nil;

  Target.RemoveListener(EventType, Callback,
    FlattenBooleanOption(Options, PROP_CAPTURE));
end;

// WHATWG DOM §2.7 EventTarget.prototype.dispatchEvent(event).
function TGocciaEventTargetValue.DispatchEventMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaEventTargetValue;
  Event: TGocciaEventValue;
begin
  if not (AThisValue is TGocciaEventTargetValue) then
    ThrowTypeError(
      'EventTarget.prototype.dispatchEvent called on incompatible receiver');
  Target := TGocciaEventTargetValue(AThisValue);

  if (AArgs.Length = 0) or not (AArgs.GetElement(0) is TGocciaEventValue) then
    ThrowTypeError(
      'EventTarget.prototype.dispatchEvent requires an Event argument');
  Event := TGocciaEventValue(AArgs.GetElement(0));
  // Step 1: throw InvalidStateError when the event's dispatch flag is set.
  if Event.DispatchFlag then
    raise TGocciaThrowValue.Create(CreateDOMExceptionObject(
      INVALID_STATE_ERROR_NAME, ALREADY_DISPATCHING_MESSAGE));

  if Target.DispatchEventValue(Event) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaEventTargetValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_EVENT_TARGET;
end;

procedure TGocciaEventTargetValue.MarkReferences;
var
  I: Integer;
  Callback: TGocciaValue;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FListeners) then
    for I := 0 to FListeners.Count - 1 do
    begin
      Callback := FListeners[I].Callback;
      if Assigned(Callback) then
        Callback.MarkReferences;
    end;
end;

initialization
  GEventTargetSharedSlot := RegisterRealmOwnedSlot('EventTarget.shared');

end.
