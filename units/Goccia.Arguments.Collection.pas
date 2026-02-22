unit Goccia.Arguments.Collection;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  // Array-like collection for function arguments
  TGocciaArgumentsCollection = class(TInterfacedObject)
  private
    FArgs: TGocciaValueList;
  public
    constructor Create; overload;
    constructor Create(const AValues: array of TGocciaValue); overload;
    destructor Destroy; override;

    // Index-based element access
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;

    // Collection operations
    function GetLength: Integer; inline;
    function IsEmpty: Boolean; inline;
    procedure Add(const AValue: TGocciaValue);
    function Slice(AStartIndex: Integer = 0; AEndIndex: Integer = -1): TGocciaArgumentsCollection;

    property Length: Integer read GetLength;
    property Items: TGocciaValueList read FArgs;
  end;

implementation

uses
  Math;

{ TGocciaArgumentsCollection }

constructor TGocciaArgumentsCollection.Create;
begin
  FArgs := TGocciaValueList.Create(False);
end;

constructor TGocciaArgumentsCollection.Create(const AValues: array of TGocciaValue);
var
  I: Integer;
begin
  FArgs := TGocciaValueList.Create(False);
  for I := 0 to High(AValues) do
    FArgs.Add(AValues[I]);
end;

destructor TGocciaArgumentsCollection.Destroy;
begin
  FArgs.Free;
  inherited;
end;

function TGocciaArgumentsCollection.GetElement(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FArgs.Count) then
    Result := FArgs[AIndex]
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArgumentsCollection.SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;
begin
  if AIndex < 0 then
  begin
    Result := False;
    Exit;
  end;

  if AIndex >= FArgs.Count then
    FArgs.Add(AValue)
  else
    FArgs[AIndex] := AValue;
  Result := True;
end;

function TGocciaArgumentsCollection.GetLength: Integer;
begin
  Result := FArgs.Count;
end;

function TGocciaArgumentsCollection.IsEmpty: Boolean;
begin
  Result := FArgs.Count = 0;
end;

procedure TGocciaArgumentsCollection.Add(const AValue: TGocciaValue);
begin
  FArgs.Add(AValue);
end;

function TGocciaArgumentsCollection.Slice(AStartIndex: Integer = 0; AEndIndex: Integer = -1): TGocciaArgumentsCollection;
var
  SlicedValues: array of TGocciaValue;
  I, ActualEnd, SliceLength: Integer;
begin
  // Handle negative or default AEndIndex (-1 means to the end)
  if AEndIndex < 0 then
    ActualEnd := FArgs.Count
  else
    ActualEnd := Min(AEndIndex, FArgs.Count);

  // Handle AStartIndex bounds
  if AStartIndex < 0 then
    AStartIndex := 0;
  if AStartIndex >= FArgs.Count then
    AStartIndex := FArgs.Count;

  // Calculate slice length
  SliceLength := Max(0, ActualEnd - AStartIndex);
  SetLength(SlicedValues, SliceLength);

  // Copy the slice
  for I := 0 to SliceLength - 1 do
    SlicedValues[I] := FArgs[AStartIndex + I];

  // Create new collection with the slice
  Result := TGocciaArgumentsCollection.Create(SlicedValues);
end;

end.
