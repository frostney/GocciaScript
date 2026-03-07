unit Goccia.Compiler.Scope;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Souffle.Bytecode.Chunk;

type
  TGocciaCompilerVariableKind = (cvkLocal, cvkUpvalue, cvkGlobal);

  TGocciaCompilerLocal = record
    Name: string;
    Slot: UInt8;
    Depth: Integer;
    IsCaptured: Boolean;
    IsConst: Boolean;
    IsGlobalBacked: Boolean;
    TypeHint: TSouffleLocalType;
    IsStrictlyTyped: Boolean;
  end;

  TGocciaCompilerUpvalue = record
    Index: UInt8;
    IsLocal: Boolean;
    IsConst: Boolean;
    TypeHint: TSouffleLocalType;
    IsStrictlyTyped: Boolean;
  end;

  TGocciaCompilerScope = class
  private
    FParent: TGocciaCompilerScope;
    FLocals: array of TGocciaCompilerLocal;
    FLocalCount: Integer;
    FUpvalues: array of TGocciaCompilerUpvalue;
    FUpvalueCount: Integer;
    FDepth: Integer;
    FNextSlot: UInt8;
    FMaxSlot: UInt8;
    FPrivatePrefix: string;
  public
    constructor Create(const AParent: TGocciaCompilerScope;
      const ADepth: Integer);

    function DeclareLocal(const AName: string;
      const AIsConst: Boolean): UInt8;
    function ResolveLocal(const AName: string): Integer;
    function ResolveUpvalue(const AName: string): Integer;
    function AddUpvalue(const AIndex: UInt8;
      const AIsLocal: Boolean; const AIsConst: Boolean = False): Integer;

    function AllocateRegister: UInt8;
    procedure FreeRegister;
    procedure BeginScope;
    procedure EndScope(out AClosedLocals: array of UInt8;
      out AClosedCount: Integer);

    property Parent: TGocciaCompilerScope read FParent;
    property LocalCount: Integer read FLocalCount;
    property UpvalueCount: Integer read FUpvalueCount;
    property Depth: Integer read FDepth;
    property MaxSlot: UInt8 read FMaxSlot;
    property NextSlot: UInt8 read FNextSlot;

    function GetLocal(const AIndex: Integer): TGocciaCompilerLocal;
    function GetUpvalue(const AIndex: Integer): TGocciaCompilerUpvalue;
    procedure MarkCaptured(const AIndex: Integer);
    procedure MarkGlobalBacked(const AIndex: Integer);
    procedure SetLocalTypeHint(const AIndex: Integer;
      const ATypeHint: TSouffleLocalType);
    procedure SetLocalStrictlyTyped(const AIndex: Integer;
      const AStrictlyTyped: Boolean);
    function ResolvePrivatePrefix: string;
    property PrivatePrefix: string read FPrivatePrefix write FPrivatePrefix;
  end;

function NextClassPrivatePrefix: string;

implementation

uses
  SysUtils;

{ TGocciaCompilerScope }

constructor TGocciaCompilerScope.Create(const AParent: TGocciaCompilerScope;
  const ADepth: Integer);
begin
  inherited Create;
  FParent := AParent;
  FLocalCount := 0;
  FUpvalueCount := 0;
  FDepth := ADepth;
  FNextSlot := 0;
  FMaxSlot := 0;
end;

function TGocciaCompilerScope.DeclareLocal(const AName: string;
  const AIsConst: Boolean): UInt8;
begin
  if FNextSlot >= High(UInt8) then
    raise Exception.Create('Compiler error: local variable slot overflow (>255)');
  if FLocalCount >= Length(FLocals) then
    SetLength(FLocals, FLocalCount * 2 + 8);
  FLocals[FLocalCount].Name := AName;
  FLocals[FLocalCount].Slot := FNextSlot;
  FLocals[FLocalCount].Depth := FDepth;
  FLocals[FLocalCount].IsCaptured := False;
  FLocals[FLocalCount].IsConst := AIsConst;
  FLocals[FLocalCount].TypeHint := sltUntyped;
  FLocals[FLocalCount].IsStrictlyTyped := False;
  Result := FNextSlot;
  Inc(FLocalCount);
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := FNextSlot;
end;

function TGocciaCompilerScope.ResolveLocal(const AName: string): Integer;
var
  I: Integer;
begin
  for I := FLocalCount - 1 downto 0 do
    if FLocals[I].Name = AName then
      Exit(I);
  Result := -1;
end;

function TGocciaCompilerScope.ResolveUpvalue(const AName: string): Integer;
var
  LocalIdx: Integer;
  UpvalueIdx: Integer;
  Idx: Integer;
begin
  if not Assigned(FParent) then
    Exit(-1);

  LocalIdx := FParent.ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    FParent.MarkCaptured(LocalIdx);
    Idx := AddUpvalue(FParent.FLocals[LocalIdx].Slot, True,
      FParent.FLocals[LocalIdx].IsConst);
    FUpvalues[Idx].TypeHint := FParent.FLocals[LocalIdx].TypeHint;
    FUpvalues[Idx].IsStrictlyTyped := FParent.FLocals[LocalIdx].IsStrictlyTyped;
    Exit(Idx);
  end;

  UpvalueIdx := FParent.ResolveUpvalue(AName);
  if UpvalueIdx >= 0 then
  begin
    if UpvalueIdx > High(UInt8) then
      raise Exception.Create('Compiler error: upvalue index overflow (>255)');
    Idx := AddUpvalue(UInt8(UpvalueIdx), False,
      FParent.FUpvalues[UpvalueIdx].IsConst);
    FUpvalues[Idx].TypeHint := FParent.FUpvalues[UpvalueIdx].TypeHint;
    FUpvalues[Idx].IsStrictlyTyped := FParent.FUpvalues[UpvalueIdx].IsStrictlyTyped;
    Exit(Idx);
  end;

  Result := -1;
end;

function TGocciaCompilerScope.AddUpvalue(const AIndex: UInt8;
  const AIsLocal: Boolean; const AIsConst: Boolean): Integer;
var
  I: Integer;
begin
  for I := 0 to FUpvalueCount - 1 do
    if (FUpvalues[I].Index = AIndex) and (FUpvalues[I].IsLocal = AIsLocal) then
      Exit(I);

  if FUpvalueCount >= High(UInt8) then
    raise Exception.Create('Compiler error: upvalue count overflow (>255)');
  if FUpvalueCount >= Length(FUpvalues) then
    SetLength(FUpvalues, FUpvalueCount * 2 + 4);
  FUpvalues[FUpvalueCount].Index := AIndex;
  FUpvalues[FUpvalueCount].IsLocal := AIsLocal;
  FUpvalues[FUpvalueCount].IsConst := AIsConst;
  FUpvalues[FUpvalueCount].TypeHint := sltUntyped;
  FUpvalues[FUpvalueCount].IsStrictlyTyped := False;
  Result := FUpvalueCount;
  Inc(FUpvalueCount);
end;

function TGocciaCompilerScope.AllocateRegister: UInt8;
begin
  if FNextSlot >= High(UInt8) then
    raise Exception.Create('Compiler error: register slot overflow (>255)');
  Result := FNextSlot;
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := FNextSlot;
end;

procedure TGocciaCompilerScope.FreeRegister;
begin
  if FNextSlot > 0 then
    Dec(FNextSlot);
end;

procedure TGocciaCompilerScope.BeginScope;
begin
  Inc(FDepth);
end;

procedure TGocciaCompilerScope.EndScope(out AClosedLocals: array of UInt8;
  out AClosedCount: Integer);
begin
  AClosedCount := 0;
  while (FLocalCount > 0) and (FLocals[FLocalCount - 1].Depth = FDepth) do
  begin
    Dec(FLocalCount);
    if FLocals[FLocalCount].IsCaptured then
    begin
      if AClosedCount >= Length(AClosedLocals) then
        raise Exception.Create('AClosedLocals buffer too small for captured locals');
      AClosedLocals[AClosedCount] := FLocals[FLocalCount].Slot;
      Inc(AClosedCount);
    end;
    Dec(FNextSlot);
  end;
  Dec(FDepth);
end;

function TGocciaCompilerScope.GetLocal(
  const AIndex: Integer): TGocciaCompilerLocal;
begin
  Result := FLocals[AIndex];
end;

function TGocciaCompilerScope.GetUpvalue(
  const AIndex: Integer): TGocciaCompilerUpvalue;
begin
  Result := FUpvalues[AIndex];
end;

procedure TGocciaCompilerScope.MarkCaptured(const AIndex: Integer);
begin
  FLocals[AIndex].IsCaptured := True;
end;

procedure TGocciaCompilerScope.MarkGlobalBacked(const AIndex: Integer);
begin
  FLocals[AIndex].IsGlobalBacked := True;
end;

procedure TGocciaCompilerScope.SetLocalTypeHint(const AIndex: Integer;
  const ATypeHint: TSouffleLocalType);
begin
  FLocals[AIndex].TypeHint := ATypeHint;
end;

procedure TGocciaCompilerScope.SetLocalStrictlyTyped(const AIndex: Integer;
  const AStrictlyTyped: Boolean);
begin
  FLocals[AIndex].IsStrictlyTyped := AStrictlyTyped;
end;

function TGocciaCompilerScope.ResolvePrivatePrefix: string;
var
  S: TGocciaCompilerScope;
begin
  S := Self;
  while Assigned(S) do
  begin
    if S.FPrivatePrefix <> '' then
      Exit(S.FPrivatePrefix);
    S := S.FParent;
  end;
  Result := '';
end;

var
  GClassPrivateCounter: Integer = 0;

function NextClassPrivatePrefix: string;
begin
  Result := IntToStr(GClassPrivateCounter) + '$';
  Inc(GClassPrivateCounter);
end;

end.
