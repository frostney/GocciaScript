unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue;

type
  TGocciaBuiltin = class
  private
    FBuiltinObjectPinned: Boolean;
  protected
    FName: string;
    FScope: TGocciaScope;
    FBuiltinObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowErrorCallback;
    procedure PinBuiltinObject;
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
  FBuiltinObjectPinned := False;
  PinBuiltinObject;
  FThrowError := AThrowError;
end;

procedure TGocciaBuiltin.PinBuiltinObject;
begin
  if FBuiltinObjectPinned or not Assigned(FBuiltinObject) then
    Exit;
  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(FBuiltinObject);
    FBuiltinObjectPinned := True;
  end;
end;

destructor TGocciaBuiltin.Destroy;
begin
  if FBuiltinObjectPinned then
  begin
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.UnpinObject(FBuiltinObject);
  end
  else
    FBuiltinObject.Free;
  inherited;
end;

end.
