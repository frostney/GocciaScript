unit Goccia.Values.EventValue;

// WHATWG DOM Event objects (https://dom.spec.whatwg.org/#interface-event).
//
// Scope: the minimal Event surface that dispatch on an EventTarget needs.
// GocciaScript has no node tree, so the propagation-only parts of the interface
// (eventPhase, stopPropagation, stopImmediatePropagation, composedPath,
// composed, isTrusted, timeStamp) are deliberately absent. `bubbles` and
// `cancelable` are recorded from EventInit and reported faithfully, but no
// propagation follows from `bubbles` because there is no tree to propagate
// through. See docs/adr/0104-whatwg-eventtarget-base.md.

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaEventValue = class(TGocciaInstanceValue)
  private
    FEventType: string;
    FTarget: TGocciaValue;
    FCurrentTarget: TGocciaValue;
    FBubbles: Boolean;
    FCancelable: Boolean;
    FDefaultPrevented: Boolean;
    FDispatchFlag: Boolean;
    function TypeGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TargetGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CurrentTargetGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BubblesGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CancelableGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DefaultPreventedGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function PreventDefault(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    function ToStringTag: string; override;
    procedure MarkReferences; override;
    class procedure ExposePrototype(const AConstructor: TGocciaValue);
    class function SharedPrototypeObject: TGocciaObjectValue;

    property EventType: string read FEventType write FEventType;
    property Target: TGocciaValue read FTarget write FTarget;
    property CurrentTarget: TGocciaValue read FCurrentTarget
      write FCurrentTarget;
    property Bubbles: Boolean read FBubbles write FBubbles;
    property Cancelable: Boolean read FCancelable write FCancelable;
    property DefaultPrevented: Boolean read FDefaultPrevented;
    property DispatchFlag: Boolean read FDispatchFlag write FDispatchFlag;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.ObjectModel,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor;

var
  GEventSharedSlot: TGocciaRealmOwnedSlotId;

function GetEventShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if CurrentRealm <> nil then
    Result := TGocciaSharedPrototype(
      CurrentRealm.GetOwnedSlot(GEventSharedSlot))
  else
    Result := nil;
end;

{ TGocciaEventValue }

constructor TGocciaEventValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FEventType := '';
  FTarget := TGocciaNullLiteralValue.NullValue;
  FCurrentTarget := TGocciaNullLiteralValue.NullValue;
  FBubbles := False;
  FCancelable := False;
  FDefaultPrevented := False;
  FDispatchFlag := False;
  InitializePrototype;
  Shared := GetEventShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaEventValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if CurrentRealm = nil then
    Exit;
  if GetEventShared <> nil then
    Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GEventSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor(PROP_TYPE, TypeGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_TARGET, TargetGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_CURRENT_TARGET, CurrentTargetGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_BUBBLES, BubblesGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_CANCELABLE, CancelableGetter, nil,
      [pfConfigurable]);
    Members.AddAccessor(PROP_DEFAULT_PREVENTED, DefaultPreventedGetter, nil,
      [pfConfigurable]);
    Members.AddNamedMethod(PROP_PREVENT_DEFAULT, PreventDefault, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaEventValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
  Bootstrap: TGocciaEventValue;
begin
  Shared := GetEventShared;
  if not Assigned(Shared) then
  begin
    Bootstrap := TGocciaEventValue.Create;
    Shared := GetEventShared;
    if not Assigned(Shared) then
      Bootstrap.Free;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

class function TGocciaEventValue.SharedPrototypeObject: TGocciaObjectValue;
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetEventShared;
  if Assigned(Shared) then
    Result := Shared.Prototype
  else
    Result := nil;
end;

// WHATWG DOM §2.2 get Event.prototype.type.
function TGocciaEventValue.TypeGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError('Event.prototype.type called on incompatible receiver');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaEventValue(AThisValue).EventType);
end;

// WHATWG DOM §2.2 get Event.prototype.target.
function TGocciaEventValue.TargetGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError('Event.prototype.target called on incompatible receiver');
  Result := TGocciaEventValue(AThisValue).Target;
end;

// WHATWG DOM §2.2 get Event.prototype.currentTarget.
function TGocciaEventValue.CurrentTargetGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError(
      'Event.prototype.currentTarget called on incompatible receiver');
  Result := TGocciaEventValue(AThisValue).CurrentTarget;
end;

// WHATWG DOM §2.2 get Event.prototype.bubbles.
function TGocciaEventValue.BubblesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError('Event.prototype.bubbles called on incompatible receiver');
  if TGocciaEventValue(AThisValue).Bubbles then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG DOM §2.2 get Event.prototype.cancelable.
function TGocciaEventValue.CancelableGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError(
      'Event.prototype.cancelable called on incompatible receiver');
  if TGocciaEventValue(AThisValue).Cancelable then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG DOM §2.2 get Event.prototype.defaultPrevented.
function TGocciaEventValue.DefaultPreventedGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError(
      'Event.prototype.defaultPrevented called on incompatible receiver');
  if TGocciaEventValue(AThisValue).DefaultPrevented then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG DOM §2.2 Event.prototype.preventDefault().
function TGocciaEventValue.PreventDefault(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Event: TGocciaEventValue;
begin
  if not (AThisValue is TGocciaEventValue) then
    ThrowTypeError(
      'Event.prototype.preventDefault called on incompatible receiver');
  Event := TGocciaEventValue(AThisValue);
  // WHATWG DOM §2.2: set the canceled flag only when the event is cancelable.
  if Event.Cancelable then
    Event.FDefaultPrevented := True;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaEventValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_EVENT;
end;

procedure TGocciaEventValue.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FTarget) then
    FTarget.MarkReferences;
  if Assigned(FCurrentTarget) then
    FCurrentTarget.MarkReferences;
end;

initialization
  GEventSharedSlot := RegisterRealmOwnedSlot('Event.shared');

end.
