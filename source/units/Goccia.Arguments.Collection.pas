unit Goccia.Arguments.Collection;

{$I Goccia.inc}

interface

uses
  Goccia.GarbageCollector,
  Goccia.Values.Primitives;

type
  // Array-like collection for function arguments.
  //
  // A live collection is a GC root source: its elements are marked for as long
  // as it exists. Without that, anything reachable only from the collection —
  // a number the VM boxed out of a raw scalar register on the way in, a value a
  // builtin read out of it and still holds — is unreachable the moment a native
  // builtin re-enters JS (a getter, a callback, a Proxy trap) and hits a GC safe
  // point. See docs/adr/0105-argument-collections-root-their-elements.md.
  TGocciaArgumentsCollection = class(TGCRootSource)
  private
    FArgs: TGocciaValueList;
  public
    constructor Create; overload;
    constructor CreateWithCapacity(const ACapacity: Integer); overload;
    constructor Create(const AValues: array of TGocciaValue); overload;
    destructor Destroy; override;

    procedure MarkRootReferences; override;

    // Index-based element access
    function GetElement(const AIndex: Integer): TGocciaValue; virtual;
    function SetElement(const AIndex: Integer; const AValue: TGocciaValue): Boolean;

    // Collection operations
    function GetLength: Integer; virtual;
    function IsEmpty: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    procedure Add(const AValue: TGocciaValue);
    procedure Clear;
    procedure EnsureCapacity(const ACapacity: Integer);
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

// FArgs is nil for subclasses whose constructors do not chain to an inherited
// one (see Goccia.Arguments.Callbacks); they supply their own slots instead.
procedure TGocciaArgumentsCollection.MarkRootReferences;
var
  I: Integer;
begin
  if not Assigned(FArgs) then
    Exit;
  for I := 0 to FArgs.Count - 1 do
    if Assigned(FArgs[I]) then
      FArgs[I].MarkReferences;
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

procedure TGocciaArgumentsCollection.Clear;
begin
  FArgs.Clear;
end;

procedure TGocciaArgumentsCollection.EnsureCapacity(const ACapacity: Integer);
begin
  if ACapacity > FArgs.Capacity then
    FArgs.Capacity := ACapacity;
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
