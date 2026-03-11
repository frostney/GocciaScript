{
  TScopeMap<TValue> - Flat-array string-keyed map for scope chains.

  Use case: Variable/environment lookups in interpreter scope chains.

  Inherits TBaseMap<string, TValue> for shared types and iteration.

  No hashing — backwards linear scan, optimal for 5-20 bindings.
  Built-in parent chain walking via Resolve/Assign/Has.
}

unit ScopeMap;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BaseMap;

type
  TScopeMap<TValue> = class(TBaseMap<string, TValue>)
  private const
    DEFAULT_CAPACITY = 8;
  private
    FNames: array of string;
    FValues: array of TValue;
    FCount: Integer;
    FCapacity: Integer;
    FParent: TScopeMap<TValue>;

    procedure EnsureCapacity; inline;
    function IndexOf(const AKey: string): Integer; inline;

  protected
    function GetCount: Integer; override;
    function GetValue(const AKey: string): TValue; override;
    procedure SetValue(const AKey: string; const AValue: TValue); override;
    function GetNextEntry(var AIterState: Integer;
      out AKey: string; out AValue: TValue): Boolean; override;

  public
    constructor Create(AParent: TScopeMap<TValue> = nil); overload;
    constructor Create(AInitialCapacity: Integer; AParent: TScopeMap<TValue> = nil); overload;
    destructor Destroy; override;

    procedure Add(const AKey: string; const AValue: TValue); override;
    function TryGetValue(const AKey: string; out AValue: TValue): Boolean; override;
    function ContainsKey(const AKey: string): Boolean; override;
    function Remove(const AKey: string): Boolean; override;
    procedure Clear; override;

    { Chain-walking operations }
    function Resolve(const AKey: string; out AValue: TValue): Boolean;
    function Assign(const AKey: string; const AValue: TValue): Boolean;
    function Has(const AKey: string): Boolean;

    property Parent: TScopeMap<TValue> read FParent write FParent;
  end;

implementation

{ Constructor / Destructor }

constructor TScopeMap<TValue>.Create(AParent: TScopeMap<TValue>);
begin
  Create(DEFAULT_CAPACITY, AParent);
end;

constructor TScopeMap<TValue>.Create(AInitialCapacity: Integer;
  AParent: TScopeMap<TValue>);
begin
  inherited Create;
  FCount := 0;
  if AInitialCapacity < DEFAULT_CAPACITY then
    AInitialCapacity := DEFAULT_CAPACITY;
  FCapacity := AInitialCapacity;
  SetLength(FNames, FCapacity);
  SetLength(FValues, FCapacity);
  FParent := AParent;
end;

destructor TScopeMap<TValue>.Destroy;
begin
  FNames := nil;
  FValues := nil;
  inherited;
end;

{ Internal }

procedure TScopeMap<TValue>.EnsureCapacity;
begin
  if FCount >= FCapacity then
  begin
    FCapacity := FCapacity * 2;
    SetLength(FNames, FCapacity);
    SetLength(FValues, FCapacity);
  end;
end;

function TScopeMap<TValue>.IndexOf(const AKey: string): Integer;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if FNames[I] = AKey then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

{ Core operations — local scope only }

procedure TScopeMap<TValue>.Add(const AKey: string; const AValue: TValue);
var
  Idx: Integer;
begin
  Idx := IndexOf(AKey);
  if Idx >= 0 then
  begin
    FValues[Idx] := AValue;
    Exit;
  end;
  EnsureCapacity;
  FNames[FCount] := AKey;
  FValues[FCount] := AValue;
  Inc(FCount);
end;

function TScopeMap<TValue>.TryGetValue(const AKey: string;
  out AValue: TValue): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOf(AKey);
  Result := Idx >= 0;
  if Result then
    AValue := FValues[Idx]
  else
    AValue := Default(TValue);
end;

function TScopeMap<TValue>.ContainsKey(const AKey: string): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TScopeMap<TValue>.Remove(const AKey: string): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOf(AKey);
  Result := Idx >= 0;
  if not Result then
    Exit;

  Dec(FCount);
  if Idx < FCount then
  begin
    FNames[Idx] := FNames[FCount];
    FValues[Idx] := FValues[FCount];
  end;
  FNames[FCount] := '';
  FValues[FCount] := Default(TValue);
end;

procedure TScopeMap<TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    FNames[I] := '';
    FValues[I] := Default(TValue);
  end;
  FCount := 0;
end;

{ Accessors }

function TScopeMap<TValue>.GetCount: Integer;
begin
  Result := FCount;
end;

function TScopeMap<TValue>.GetValue(const AKey: string): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise Exception.Create('Key not found in scope map');
end;

procedure TScopeMap<TValue>.SetValue(const AKey: string;
  const AValue: TValue);
begin
  Add(AKey, AValue);
end;

{ Iteration }

function TScopeMap<TValue>.GetNextEntry(var AIterState: Integer;
  out AKey: string; out AValue: TValue): Boolean;
begin
  if AIterState < FCount then
  begin
    AKey := FNames[AIterState];
    AValue := FValues[AIterState];
    Inc(AIterState);
    Result := True;
  end
  else
    Result := False;
end;

{ Chain-walking operations }

function TScopeMap<TValue>.Resolve(const AKey: string;
  out AValue: TValue): Boolean;
var
  Current: TScopeMap<TValue>;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    if Current.TryGetValue(AKey, AValue) then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.FParent;
  end;
  AValue := Default(TValue);
  Result := False;
end;

function TScopeMap<TValue>.Assign(const AKey: string;
  const AValue: TValue): Boolean;
var
  Current: TScopeMap<TValue>;
  Idx: Integer;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    Idx := Current.IndexOf(AKey);
    if Idx >= 0 then
    begin
      Current.FValues[Idx] := AValue;
      Result := True;
      Exit;
    end;
    Current := Current.FParent;
  end;
  Result := False;
end;

function TScopeMap<TValue>.Has(const AKey: string): Boolean;
var
  Current: TScopeMap<TValue>;
begin
  Current := Self;
  while Assigned(Current) do
  begin
    if Current.ContainsKey(AKey) then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.FParent;
  end;
  Result := False;
end;

end.
