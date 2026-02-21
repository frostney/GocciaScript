unit Goccia.Scope.BindingMap;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaStringArray = array of string;

  TGocciaDeclarationType = (dtLet, dtConst, dtParameter);

  TLexicalBinding = record
  private
    function IsWritable: Boolean;
    function CanAccess: Boolean;
  public
    Value: TGocciaValue;
    DeclarationType: TGocciaDeclarationType;
    Initialized: Boolean;

    property Writable: Boolean read IsWritable;
    property IsAccessible: Boolean read CanAccess;
  end;

const
  BINDING_MAP_DEFAULT_CAPACITY = 4;

type
  TGocciaBindingMap = class
  private
    FKeys: array of string;
    FValues: array of TGocciaValue;
    FDeclTypes: array of TGocciaDeclarationType;
    FInitialized: array of Boolean;
    FCount: Integer;
    FCapacity: Integer;
    function GetValueAt(const AIndex: Integer): TGocciaValue; inline;
    procedure Grow;
  public
    constructor Create(const ACapacity: Integer = BINDING_MAP_DEFAULT_CAPACITY);
    destructor Destroy; override;

    function IndexOf(const AKey: string): Integer; inline;
    function TryGetValue(const AKey: string; out ABinding: TLexicalBinding): Boolean;
    procedure AddOrSetValue(const AKey: string; const ABinding: TLexicalBinding);
    function ContainsKey(const AKey: string): Boolean; inline;
    function GetKeys: TGocciaStringArray;

    property Values[AIndex: Integer]: TGocciaValue read GetValueAt; default;
    property Count: Integer read FCount;
  end;

implementation

{ TLexicalBinding }

function TLexicalBinding.IsWritable: Boolean;
begin
  Result := DeclarationType in [dtLet, dtParameter];
end;

function TLexicalBinding.CanAccess: Boolean;
begin
  Result := Initialized or (DeclarationType = dtParameter);
end;

{ TGocciaBindingMap }

constructor TGocciaBindingMap.Create(const ACapacity: Integer = BINDING_MAP_DEFAULT_CAPACITY);
begin
  // TODO: Typically, we won't need capacity < 4, but we should still support it at some point
  if ACapacity >= BINDING_MAP_DEFAULT_CAPACITY then
    FCapacity := ACapacity
  else
    FCapacity := BINDING_MAP_DEFAULT_CAPACITY;
  FCount := 0;
  SetLength(FKeys, FCapacity);
  SetLength(FValues, FCapacity);
  SetLength(FDeclTypes, FCapacity);
  SetLength(FInitialized, FCapacity);
end;

destructor TGocciaBindingMap.Destroy;
begin
  FKeys := nil;
  FValues := nil;
  FDeclTypes := nil;
  FInitialized := nil;
  inherited;
end;

function TGocciaBindingMap.IndexOf(const AKey: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FKeys[I] = AKey then
      Exit(I);
  Result := -1;
end;

procedure TGocciaBindingMap.Grow;
var
  NewCapacity: Integer;
begin
  NewCapacity := FCapacity * 2;
  SetLength(FKeys, NewCapacity);
  SetLength(FValues, NewCapacity);
  SetLength(FDeclTypes, NewCapacity);
  SetLength(FInitialized, NewCapacity);
  FCapacity := NewCapacity;
end;

function TGocciaBindingMap.TryGetValue(const AKey: string; out ABinding: TLexicalBinding): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOf(AKey);
  if Idx >= 0 then
  begin
    ABinding.Value := FValues[Idx];
    ABinding.DeclarationType := FDeclTypes[Idx];
    ABinding.Initialized := FInitialized[Idx];
    Result := True;
  end
  else
    Result := False;
end;

procedure TGocciaBindingMap.AddOrSetValue(const AKey: string; const ABinding: TLexicalBinding);
var
  Idx: Integer;
begin
  Idx := IndexOf(AKey);
  if Idx >= 0 then
  begin
    FValues[Idx] := ABinding.Value;
    FDeclTypes[Idx] := ABinding.DeclarationType;
    FInitialized[Idx] := ABinding.Initialized;
  end
  else
  begin
    if FCount = FCapacity then
      Grow;
    FKeys[FCount] := AKey;
    FValues[FCount] := ABinding.Value;
    FDeclTypes[FCount] := ABinding.DeclarationType;
    FInitialized[FCount] := ABinding.Initialized;
    Inc(FCount);
  end;
end;

function TGocciaBindingMap.ContainsKey(const AKey: string): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TGocciaBindingMap.GetKeys: TGocciaStringArray;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  for I := 0 to FCount - 1 do
    Result[I] := FKeys[I];
end;

function TGocciaBindingMap.GetValueAt(const AIndex: Integer): TGocciaValue;
begin
  Result := FValues[AIndex];
end;

end.
