unit Goccia.Values.WeakRefValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaWeakRefValue = class(TGocciaInstanceValue)
  private
    FTarget: TGocciaValue;
    procedure InitializePrototype;
  public
    function WeakRefDeref(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    function ToStringTag: string; override;
    function UsesECMAScriptBuiltinTagFallback: Boolean; override;

    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;
    procedure SweepWeakReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Target: TGocciaValue read FTarget;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.WeakReferenceSupport;

var
  GWeakRefSharedSlot: TGocciaRealmOwnedSlotId;

function GetWeakRefShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GWeakRefSharedSlot))
  else
    Result := nil;
end;

constructor TGocciaWeakRefValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FTarget := nil;
  InitializePrototype;
  Shared := GetWeakRefShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaWeakRefValue.Destroy;
begin
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnregisterWeakContainer(Self);
  inherited;
end;

procedure TGocciaWeakRefValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetWeakRefShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GWeakRefSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('deref', WeakRefDeref, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype, gmfNotConstructable]);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_WEAK_REF),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaWeakRefValue.ExposePrototype(
  const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetWeakRefShared;
  if not Assigned(Shared) then
  begin
    TGocciaWeakRefValue.Create;
    Shared := GetWeakRefShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaWeakRefValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_WEAK_REF;
end;

function TGocciaWeakRefValue.UsesECMAScriptBuiltinTagFallback: Boolean;
begin
  Result := True;
end;

// ES2026 §26.1.1.1 WeakRef(target)
procedure TGocciaWeakRefValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
begin
  FTarget := AArguments.GetElement(0);
  RequireCanBeHeldWeakly(FTarget, CONSTRUCTOR_WEAK_REF);
  // A live WeakRef holds a weak target the GC must clear when it dies, so
  // count it as a weak container (the prototype host has no target).
  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.AddKeptObject(FTarget);
    TGarbageCollector.Instance.RegisterWeakContainer(Self);
  end;
end;

procedure TGocciaWeakRefValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

procedure TGocciaWeakRefValue.SweepWeakReferences;
begin
  if Assigned(FTarget) and not FTarget.GCMarked then
    FTarget := nil;
end;

// ES2026 §26.1.3.2 WeakRef.prototype.deref()
function TGocciaWeakRefValue.WeakRefDeref(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  WeakRef: TGocciaWeakRefValue;
begin
  if not (AThisValue is TGocciaWeakRefValue) then
    ThrowTypeError(SErrorWeakRefDerefNonWeakRef, SSuggestWeakRefThisType);

  WeakRef := TGocciaWeakRefValue(AThisValue);
  if Assigned(WeakRef.FTarget) then
  begin
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddKeptObject(WeakRef.FTarget);
    Exit(WeakRef.FTarget);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

initialization
  GWeakRefSharedSlot := RegisterRealmOwnedSlot('WeakRef.shared');

end.
