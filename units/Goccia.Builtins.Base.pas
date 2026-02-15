unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Scope, SysUtils, Generics.Collections, Goccia.Arguments.Collection, Goccia.Values.Primitives;

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

uses Goccia.GarbageCollector;

constructor TGocciaBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  FName := AName;
  FScope := AScope;
  FBuiltinObject := TGocciaObjectValue.Create;
  FThrowError := AThrowError;
end;

destructor TGocciaBuiltin.Destroy;
begin
  // FBuiltinObject is GC-managed when GC is active; only free manually otherwise
  if not Assigned(TGocciaGC.Instance) then
    FBuiltinObject.Free;
  inherited;
end;

end.
