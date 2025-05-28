unit Goccia.Scope;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.Undefined, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Error, Goccia.Interfaces, SysUtils;

type
  // TODO: Add a way to access thisArg
  // TODO: Do we want to differentiate between global, local and function scopes?
  TGocciaScope = class
  private
    FValues: TDictionary<string, TGocciaValue>;
    FParent: TGocciaScope;
    FInterpreter: IGocciaInterpreter;
    FThisValue: TGocciaValue;
  public
    constructor Create(AInterpreter: IGocciaInterpreter; AParent: TGocciaScope = nil);
    destructor Destroy; override;
    function CreateChild: TGocciaScope;
    function GetValue(const AName: string): TGocciaValue;
    procedure Assign(const AName: string; AValue: TGocciaValue);
    function Contains(const AName: string): Boolean;
    property Parent: TGocciaScope read FParent;
    property Interpreter: IGocciaInterpreter read FInterpreter;
    procedure SetValue(const AName: string; AValue: TGocciaValue);
    property ThisValue: TGocciaValue read FThisValue write FThisValue;
  end;


implementation

constructor TGocciaScope.Create(AInterpreter: IGocciaInterpreter; AParent: TGocciaScope = nil);
begin
  WriteLn('Scope.Create: Creating new scope');
  WriteLn('Scope.Create: Parent scope address: ', PtrUInt(AParent));
  System.Flush(Output);

  FThisValue := TGocciaUndefinedValue.Create;
  FInterpreter := AInterpreter;
  FParent := AParent;
  FValues := TDictionary<string, TGocciaValue>.Create;
end;

destructor TGocciaScope.Destroy;
begin
  WriteLn('Scope.Destroy: Destroying scope');
  WriteLn('Scope.Destroy: Self address: ', PtrUInt(Self));
  WriteLn('Scope.Destroy: Values dictionary address: ', PtrUInt(FValues));
  System.Flush(Output);

  if Assigned(FValues) then
  begin
    FValues.Free;
    FValues := nil;
  end;

  inherited;
end;

function TGocciaScope.CreateChild: TGocciaScope;
begin
  Result := TGocciaScope.Create(FInterpreter, Self);
end;

function TGocciaScope.GetValue(const AName: string): TGocciaValue;
var
  Value: TGocciaValue;
begin
  WriteLn('Scope.GetValue: Attempting to get value for name: ', AName);
  WriteLn('Scope.GetValue: Self address: ', PtrUInt(Self));
  WriteLn('Scope.GetValue: Values dictionary address: ', PtrUInt(FValues));
  WriteLn('Scope.GetValue: ThisValue: ', ThisValue.ToString);
  System.Flush(Output);

  // Check if this value is an object and check there first
  if (ThisValue is TGocciaObjectValue) then
  begin
    Result := TGocciaObjectValue(ThisValue).GetProperty(AName);
    Exit;
  end;

  if FValues.TryGetValue(AName, Value) then
  begin
    WriteLn('Scope.GetValue: Found value of type: ', Value.ClassName);
    System.Flush(Output);
    Result := Value;
  end
  else if Assigned(FParent) then
  begin
    WriteLn('Scope.GetValue: Value not found, checking enclosing scope');
    System.Flush(Output);
    Result := FParent.GetValue(AName);
  end
  else
  begin
    WriteLn('Scope.Get: Value not found and no enclosing scope');
    System.Flush(Output);
    Result := TGocciaUndefinedValue.Create;
  end;
end;

procedure TGocciaScope.Assign(const AName: string; AValue: TGocciaValue);
begin
  // TODO: Do we nned this?
  if FValues.ContainsKey(AName) then
    FValues.AddOrSetValue(AName, AValue)
  else if Assigned(FParent) then
    FParent.Assign(AName, AValue)
  else
    raise TGocciaReferenceError.Create(Format('Undefined variable: %s', [AName]),
      0, 0, '', nil);
end;

function TGocciaScope.Contains(const AName: string): Boolean;
begin
  Result := FValues.ContainsKey(AName) or
    (Assigned(FParent) and FParent.Contains(AName));
end;

procedure TGocciaScope.SetValue(const AName: string; AValue: TGocciaValue);
begin
  WriteLn('Scope.SetValue: Setting value for name: ', AName);
  WriteLn('Scope.SetValue: Value type: ', AValue.ClassName);
  WriteLn('Scope.SetValue: Self address: ', PtrUInt(Self));
  WriteLn('Scope.SetValue: Values dictionary address: ', PtrUInt(FValues));
  System.Flush(Output);

  FValues.AddOrSetValue(AName, AValue);
end;

end.
