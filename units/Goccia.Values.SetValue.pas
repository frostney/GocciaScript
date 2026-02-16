unit Goccia.Values.SetValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction, Goccia.Arguments.Collection,
  Generics.Collections, SysUtils;

type
  TGocciaSetValue = class(TGocciaObjectValue)
  private
    class var FSharedSetPrototype: TGocciaObjectValue;
    class var FPrototypeMethodHost: TGocciaSetValue;
  private
    FItems: TList<TGocciaValue>;

    function SetHas(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SetAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SetDelete(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SetClear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SetForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function SetValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;

    function ContainsValue(AValue: TGocciaValue): Boolean;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure InitializePrototype;

    procedure AddItem(AValue: TGocciaValue);

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToArray: TGocciaArrayValue;
    function ToStringTag: string; override;

    procedure GCMarkReferences; override;

    property Items: TList<TGocciaValue> read FItems;
  end;

implementation

uses
  Goccia.Evaluator.Comparison, Goccia.Values.FunctionValue,
  Goccia.Values.FunctionBase, Goccia.GarbageCollector;

constructor TGocciaSetValue.Create;
begin
  inherited Create(nil);
  FItems := TList<TGocciaValue>.Create;
  InitializePrototype;
  if Assigned(FSharedSetPrototype) then
    FPrototype := FSharedSetPrototype;
end;

procedure TGocciaSetValue.InitializePrototype;
begin
  if Assigned(FSharedSetPrototype) then Exit;

  FSharedSetPrototype := TGocciaObjectValue.Create;
  FPrototypeMethodHost := Self;

  FSharedSetPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetHas, 'has', 1));
  FSharedSetPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetAdd, 'add', 1));
  FSharedSetPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetDelete, 'delete', 1));
  FSharedSetPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetClear, 'clear', 0));
  FSharedSetPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetForEach, 'forEach', 1));
  FSharedSetPrototype.RegisterNativeMethod(TGocciaNativeFunctionValue.CreateWithoutPrototype(SetValues, 'values', 0));

  if Assigned(TGocciaGC.Instance) then
  begin
    TGocciaGC.Instance.PinValue(FSharedSetPrototype);
    TGocciaGC.Instance.PinValue(FPrototypeMethodHost);
  end;
end;

destructor TGocciaSetValue.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TGocciaSetValue.GCMarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited; // Marks self + object properties/prototype

  // Mark all set items
  for I := 0 to FItems.Count - 1 do
  begin
    if Assigned(FItems[I]) then
      FItems[I].GCMarkReferences;
  end;
end;

function TGocciaSetValue.ContainsValue(AValue: TGocciaValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    if IsSameValueZero(FItems[I], AValue) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TGocciaSetValue.AddItem(AValue: TGocciaValue);
begin
  if not ContainsValue(AValue) then
    FItems.Add(AValue);
end;

function TGocciaSetValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = 'size' then
    Result := TGocciaNumberLiteralValue.Create(FItems.Count)
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaSetValue.ToArray: TGocciaArrayValue;
var
  I: Integer;
begin
  Result := TGocciaArrayValue.Create;
  for I := 0 to FItems.Count - 1 do
    Result.Elements.Add(FItems[I]);
end;

function TGocciaSetValue.ToStringTag: string;
begin
  Result := 'Set';
end;

{ Instance methods }

function TGocciaSetValue.SetHas(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  S := TGocciaSetValue(ThisValue);
  if Args.Length > 0 then
    Result := TGocciaBooleanLiteralValue.Create(S.ContainsValue(Args.GetElement(0)))
  else
    Result := TGocciaBooleanLiteralValue.Create(False);
end;

function TGocciaSetValue.SetAdd(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
begin
  S := TGocciaSetValue(ThisValue);
  if Args.Length > 0 then
    S.AddItem(Args.GetElement(0));
  Result := ThisValue;
end;

function TGocciaSetValue.SetDelete(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  I: Integer;
begin
  S := TGocciaSetValue(ThisValue);
  Result := TGocciaBooleanLiteralValue.Create(False);
  if Args.Length > 0 then
  begin
    for I := 0 to S.FItems.Count - 1 do
    begin
      if IsSameValueZero(S.FItems[I], Args.GetElement(0)) then
      begin
        S.FItems.Delete(I);
        Result := TGocciaBooleanLiteralValue.Create(True);
        Exit;
      end;
    end;
  end;
end;

function TGocciaSetValue.SetClear(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaSetValue(ThisValue).FItems.Clear;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSetValue.SetForEach(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  S: TGocciaSetValue;
  Callback: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Args.Length = 0 then Exit;

  S := TGocciaSetValue(ThisValue);
  Callback := Args.GetElement(0);
  if not (Callback is TGocciaFunctionBase) then Exit;

  for I := 0 to S.FItems.Count - 1 do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([S.FItems[I], S.FItems[I], ThisValue]);
    try
      TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;
  end;
end;

function TGocciaSetValue.SetValues(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSetValue(ThisValue).ToArray;
end;

end.
