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
  if TGocciaObjectValue.SharedObjectPrototype = nil then
    TGocciaObjectValue.InitializeSharedPrototype;
  FBuiltinObject := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  FThrowError := AThrowError;
end;

destructor TGocciaBuiltin.Destroy;
begin
  // FBuiltinObject is GC-managed when GC is active; only free manually otherwise
  if (TGarbageCollector.Instance = nil) then
    FBuiltinObject.Free;
  inherited;
end;

end.
