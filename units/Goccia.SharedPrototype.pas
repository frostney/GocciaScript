unit Goccia.SharedPrototype;

{$I Goccia.inc}

interface

uses
  SyncObjs,

  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

var
  { Critical section protecting writes to shared prototype objects from
    concurrent threads. Used by ExposeOnConstructor and the older
    ExposePrototype class methods (e.g. TGocciaArrayValue). }
  GSharedPrototypeLock: TCriticalSection;

type
  TGocciaSharedPrototype = class
  private
    FPrototype: TGocciaObjectValue;
    FMethodHost: TGocciaValue;
  public
    constructor Create(const AMethodHost: TGocciaValue);
    procedure ExposeOnConstructor(const AConstructor: TGocciaValue);
    property Prototype: TGocciaObjectValue read FPrototype;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ClassValue;

constructor TGocciaSharedPrototype.Create(const AMethodHost: TGocciaValue);
begin
  inherited Create;
  FPrototype := TGocciaObjectValue.Create;
  FMethodHost := AMethodHost;

  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FPrototype);
    if Assigned(FMethodHost) then
      TGarbageCollector.Instance.PinObject(FMethodHost);
  end;
end;

procedure TGocciaSharedPrototype.ExposeOnConstructor(const AConstructor: TGocciaValue);
begin
  if AConstructor is TGocciaClassValue then
    TGocciaClassValue(AConstructor).ReplacePrototype(FPrototype)
  else if AConstructor is TGocciaObjectValue then
    TGocciaObjectValue(AConstructor).AssignProperty(PROP_PROTOTYPE, FPrototype);
  // Only write constructor to the shared prototype if not yet set.
  // After EnsureSharedPrototypesInitialized runs on the main thread,
  // this property is already populated. Skipping the write on worker
  // threads avoids a data race on the prototype's property hash map.
  if not FPrototype.HasProperty(PROP_CONSTRUCTOR) then
    FPrototype.AssignProperty(PROP_CONSTRUCTOR, AConstructor);
end;

initialization
  GSharedPrototypeLock := TCriticalSection.Create;

finalization
  GSharedPrototypeLock.Free;

end.
