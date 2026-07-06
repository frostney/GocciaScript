unit Goccia.Arguments.Collection;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  // Array-like collection for function arguments
  TGocciaArgumentsCollection = class(TInterfacedObject)
  protected
    FArgs: TGocciaValueList;
    function GetItems: TGocciaValueList; virtual;
  public
    constructor Create; overload;
    constructor CreateWithCapacity(const ACapacity: Integer); overload;
    constructor Create(const AValues: array of TGocciaValue); overload;
    destructor Destroy; override;

    // Index-based element access
    function GetElement(const AIndex: Integer): TGocciaValue; virtual;
    function SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;

    // Collection operations
    function GetLength: Integer; virtual;
    function IsEmpty: Boolean; inline;
    procedure Add(const AValue: TGocciaValue);
    procedure Clear;
    procedure EnsureCapacity(const ACapacity: Integer);
    function Slice(AStartIndex: Integer = 0; AEndIndex: Integer = -1): TGocciaArgumentsCollection;

    property Length: Integer read GetLength;
    property Items: TGocciaValueList read GetItems;
  end;

  TGocciaFixedArgumentsCollection = class(TGocciaArgumentsCollection)
  private
    FArg0: TGocciaValue;
    FArg1: TGocciaValue;
    FArg2: TGocciaValue;
    FLength: Integer;
    procedure MaterializeItems;
  protected
    function GetItems: TGocciaValueList; override;
  public
    constructor CreateNoArgs;
    constructor CreateOneArg(const AArg0: TGocciaValue);
    constructor CreateTwoArgs(const AArg0, AArg1: TGocciaValue);
    constructor CreateThreeArgs(const AArg0, AArg1, AArg2: TGocciaValue);

    function GetElement(const AIndex: Integer): TGocciaValue; override;
    function GetLength: Integer; override;
  end;

implementation

uses
  Math;

{ TGocciaArgumentsCollection }

constructor TGocciaArgumentsCollection.Create;
begin
  FArgs := TGocciaValueList.Create(False);
end;

constructor TGocciaArgumentsCollection.CreateWithCapacity(
  const ACapacity: Integer);
begin
  FArgs := TGocciaValueList.Create(False);
  if ACapacity > 0 then
    FArgs.Capacity := ACapacity;
end;

constructor TGocciaArgumentsCollection.Create(const AValues: array of TGocciaValue);
var
  I: Integer;
begin
  FArgs := TGocciaValueList.Create(False);
  if High(AValues) >= 0 then
    FArgs.Capacity := High(AValues) + 1;
  for I := 0 to High(AValues) do
    FArgs.Add(AValues[I]);
end;

destructor TGocciaArgumentsCollection.Destroy;
begin
  FArgs.Free;
  inherited;
end;

function TGocciaArgumentsCollection.GetItems: TGocciaValueList;
begin
  Result := FArgs;
end;

function TGocciaArgumentsCollection.GetElement(const AIndex: Integer): TGocciaValue;
var
  Args: TGocciaValueList;
begin
  Args := GetItems;
  if (AIndex >= 0) and (AIndex < Args.Count) then
    Result := Args[AIndex]
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArgumentsCollection.SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;
var
  Args: TGocciaValueList;
begin
  if AIndex < 0 then
  begin
    Result := False;
    Exit;
  end;

  Args := GetItems;
  if AIndex >= Args.Count then
    Args.Add(AValue)
  else
    Args[AIndex] := AValue;
  Result := True;
end;

function TGocciaArgumentsCollection.GetLength: Integer;
begin
  Result := GetItems.Count;
end;

function TGocciaArgumentsCollection.IsEmpty: Boolean;
begin
  Result := GetLength = 0;
end;

procedure TGocciaArgumentsCollection.Add(const AValue: TGocciaValue);
begin
  GetItems.Add(AValue);
end;

procedure TGocciaArgumentsCollection.Clear;
begin
  GetItems.Clear;
end;

procedure TGocciaArgumentsCollection.EnsureCapacity(const ACapacity: Integer);
begin
  if ACapacity > GetItems.Capacity then
    GetItems.Capacity := ACapacity;
end;

function TGocciaArgumentsCollection.Slice(AStartIndex: Integer = 0; AEndIndex: Integer = -1): TGocciaArgumentsCollection;
var
  Args: TGocciaValueList;
  SlicedValues: array of TGocciaValue;
  I, ActualEnd, SliceLength: Integer;
begin
  Args := GetItems;
  // Handle negative or default AEndIndex (-1 means to the end)
  if AEndIndex < 0 then
    ActualEnd := Args.Count
  else
    ActualEnd := Min(AEndIndex, Args.Count);

  // Handle AStartIndex bounds
  if AStartIndex < 0 then
    AStartIndex := 0;
  if AStartIndex >= Args.Count then
    AStartIndex := Args.Count;

  // Calculate slice length
  SliceLength := Max(0, ActualEnd - AStartIndex);
  SetLength(SlicedValues, SliceLength);

  // Copy the slice
  for I := 0 to SliceLength - 1 do
    SlicedValues[I] := Args[AStartIndex + I];

  // Create new collection with the slice
  Result := TGocciaArgumentsCollection.Create(SlicedValues);
end;

{ TGocciaFixedArgumentsCollection }

constructor TGocciaFixedArgumentsCollection.CreateNoArgs;
begin
  FLength := 0;
end;

constructor TGocciaFixedArgumentsCollection.CreateOneArg(
  const AArg0: TGocciaValue);
begin
  FArg0 := AArg0;
  FLength := 1;
end;

constructor TGocciaFixedArgumentsCollection.CreateTwoArgs(
  const AArg0, AArg1: TGocciaValue);
begin
  FArg0 := AArg0;
  FArg1 := AArg1;
  FLength := 2;
end;

constructor TGocciaFixedArgumentsCollection.CreateThreeArgs(
  const AArg0, AArg1, AArg2: TGocciaValue);
begin
  FArg0 := AArg0;
  FArg1 := AArg1;
  FArg2 := AArg2;
  FLength := 3;
end;

procedure TGocciaFixedArgumentsCollection.MaterializeItems;
begin
  if Assigned(FArgs) then
    Exit;

  FArgs := TGocciaValueList.Create(False);
  if FLength > 0 then
    FArgs.Capacity := FLength;
  if FLength >= 1 then
    FArgs.Add(FArg0);
  if FLength >= 2 then
    FArgs.Add(FArg1);
  if FLength >= 3 then
    FArgs.Add(FArg2);
end;

function TGocciaFixedArgumentsCollection.GetItems: TGocciaValueList;
begin
  MaterializeItems;
  Result := FArgs;
end;

function TGocciaFixedArgumentsCollection.GetElement(
  const AIndex: Integer): TGocciaValue;
begin
  if Assigned(FArgs) then
    Exit(inherited GetElement(AIndex));

  if (AIndex < 0) or (AIndex >= FLength) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  case AIndex of
    0: Result := FArg0;
    1: Result := FArg1;
    2: Result := FArg2;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaFixedArgumentsCollection.GetLength: Integer;
begin
  if Assigned(FArgs) then
    Result := inherited GetLength
  else
    Result := FLength;
end;

end.
