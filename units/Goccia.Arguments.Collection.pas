unit Goccia.Arguments.Collection;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.Interfaces, Generics.Collections, SysUtils, Math;

type
  // Array-like collection for function arguments
  TGocciaArgumentsCollection = class(TInterfacedObject, IIndexMethods)
  private
    FArgs: TObjectList<TGocciaValue>;
  public
    constructor Create; overload;
    constructor Create(const Values: array of TGocciaValue); overload;
    destructor Destroy; override;

    // IIndexMethods implementation
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; AValue: TGocciaValue): Boolean;

    // Collection operations
    function GetLength: Integer; inline;
    function IsEmpty: Boolean; inline;
    procedure Add(Value: TGocciaValue);
    function Slice(StartIndex: Integer = 0; EndIndex: Integer = -1): TGocciaArgumentsCollection;

    property Length: Integer read GetLength;
  end;

implementation

{ TGocciaArgumentsCollection }

constructor TGocciaArgumentsCollection.Create;
begin
  FArgs := TObjectList<TGocciaValue>.Create(False);
end;

constructor TGocciaArgumentsCollection.Create(const Values: array of TGocciaValue);
var
  I: Integer;
begin
  FArgs := TObjectList<TGocciaValue>.Create(False);
  for I := 0 to High(Values) do
    FArgs.Add(Values[I]);
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

function TGocciaArgumentsCollection.SetElement(const AIndex: Integer; AValue: TGocciaValue): Boolean;
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

procedure TGocciaArgumentsCollection.Add(Value: TGocciaValue);
begin
  FArgs.Add(Value);
end;

function TGocciaArgumentsCollection.Slice(StartIndex: Integer = 0; EndIndex: Integer = -1): TGocciaArgumentsCollection;
var
  SlicedValues: array of TGocciaValue;
  I, ActualEnd, SliceLength: Integer;
begin
  // Handle negative or default EndIndex (-1 means to the end)
  if EndIndex < 0 then
    ActualEnd := FArgs.Count
  else
    ActualEnd := Min(EndIndex, FArgs.Count);

  // Handle StartIndex bounds
  if StartIndex < 0 then
    StartIndex := 0;
  if StartIndex >= FArgs.Count then
    StartIndex := FArgs.Count;

  // Calculate slice length
  SliceLength := Max(0, ActualEnd - StartIndex);
  SetLength(SlicedValues, SliceLength);

  // Copy the slice
  for I := 0 to SliceLength - 1 do
    SlicedValues[I] := FArgs[StartIndex + I];

  // Create new collection with the slice
  Result := TGocciaArgumentsCollection.Create(SlicedValues);
end;

end.
