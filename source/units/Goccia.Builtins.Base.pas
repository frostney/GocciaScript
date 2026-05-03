unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue;

type
  TGocciaBuiltin = class
  protected
    FName: string;
    FScope: TGocciaScope;
    FBuiltinObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowErrorCallback;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;

    property Name: string read FName;
    property BuiltinObject: TGocciaObjectValue read FBuiltinObject;
    property ThrowError: TGocciaThrowErrorCallback read FThrowError;
  end;

implementation

uses
  Goccia.GarbageCollector;

constructor TGocciaBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  FName := AName;
  FScope := AScope;
  if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    TGocciaObjectValue.InitializeSharedPrototype;
  FBuiltinObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(FBuiltinObject);
  FThrowError := AThrowError;
end;

destructor TGocciaBuiltin.Destroy;
begin
  // FBuiltinObject is GC-managed when GC is active; only free manually otherwise.
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnpinObject(FBuiltinObject)
  else
    FBuiltinObject.Free;
  inherited;
end;

end.
