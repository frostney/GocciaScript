unit Goccia.Bytecode.Debug;

{$I Goccia.inc}

interface

type
  TGocciaLineMapEntry = record
    PC: UInt32;
    Line: UInt32;
    Column: UInt16;
  end;

  TGocciaLocalInfo = record
    Name: string;
    Slot: UInt16;
    StartPC: UInt32;
    EndPC: UInt32;
  end;

  TGocciaDebugInfo = class
  private
    FSourceFile: string;
    FLineMap: array of TGocciaLineMapEntry;
    FLineMapCount: Integer;
    FLocals: array of TGocciaLocalInfo;
    FLocalCount: Integer;
    FDeclarationLine: UInt32;
    FDeclarationColumn: UInt16;
  public
    (* ADeclarationLine/ADeclarationColumn locate the function's declaration
       (`const f = () => {`), which is what LCOV's FN: record means. They are
       distinct from the first line-map entry, which locates the first executed
       instruction of the body and drives per-call line hits. Zero means
       "not recorded" — the module-level template has no declaration site. *)
    constructor Create(const ASourceFile: string;
      const ADeclarationLine: UInt32 = 0;
      const ADeclarationColumn: UInt16 = 0);

    procedure AddLineMapping(const APC: UInt32; const ALine: UInt32;
      const AColumn: UInt16);
    procedure AddLocal(const AName: string; const ASlot: UInt16;
      const AStartPC, AEndPC: UInt32);

    function GetLineForPC(const APC: UInt32): UInt32;
    function GetColumnForPC(const APC: UInt32): UInt16;

    function GetLineMapEntry(const AIndex: Integer): TGocciaLineMapEntry;
    function GetLocalInfo(const AIndex: Integer): TGocciaLocalInfo;

    { Declaration position, falling back to the first line-map entry when the
      declaration site was not recorded, so callers always get a usable
      position. }
    function CoverageLine: UInt32;
    function CoverageColumn: UInt16;

    property SourceFile: string read FSourceFile;
    property LineMapCount: Integer read FLineMapCount;
    property LocalCount: Integer read FLocalCount;
    property DeclarationLine: UInt32 read FDeclarationLine
      write FDeclarationLine;
    property DeclarationColumn: UInt16 read FDeclarationColumn
      write FDeclarationColumn;
  end;

implementation

constructor TGocciaDebugInfo.Create(const ASourceFile: string;
  const ADeclarationLine: UInt32 = 0;
  const ADeclarationColumn: UInt16 = 0);
begin
  inherited Create;
  FSourceFile := ASourceFile;
  FLineMapCount := 0;
  FLocalCount := 0;
  FDeclarationLine := ADeclarationLine;
  FDeclarationColumn := ADeclarationColumn;
end;

function TGocciaDebugInfo.CoverageLine: UInt32;
begin
  if FDeclarationLine > 0 then
    Result := FDeclarationLine
  else if FLineMapCount > 0 then
    Result := FLineMap[0].Line
  else
    Result := 0;
end;

function TGocciaDebugInfo.CoverageColumn: UInt16;
begin
  if FDeclarationLine > 0 then
    Result := FDeclarationColumn
  else if FLineMapCount > 0 then
    Result := FLineMap[0].Column
  else
    Result := 0;
end;

procedure TGocciaDebugInfo.AddLineMapping(const APC: UInt32;
  const ALine: UInt32; const AColumn: UInt16);
begin
  if FLineMapCount >= Length(FLineMap) then
    SetLength(FLineMap, FLineMapCount * 2 + 8);
  FLineMap[FLineMapCount].PC := APC;
  FLineMap[FLineMapCount].Line := ALine;
  FLineMap[FLineMapCount].Column := AColumn;
  Inc(FLineMapCount);
end;

procedure TGocciaDebugInfo.AddLocal(const AName: string; const ASlot: UInt16;
  const AStartPC, AEndPC: UInt32);
begin
  if FLocalCount >= Length(FLocals) then
    SetLength(FLocals, FLocalCount * 2 + 4);
  FLocals[FLocalCount].Name := AName;
  FLocals[FLocalCount].Slot := ASlot;
  FLocals[FLocalCount].StartPC := AStartPC;
  FLocals[FLocalCount].EndPC := AEndPC;
  Inc(FLocalCount);
end;

function TGocciaDebugInfo.GetLineForPC(const APC: UInt32): UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := FLineMapCount - 1 downto 0 do
    if FLineMap[I].PC <= APC then
      Exit(FLineMap[I].Line);
end;

function TGocciaDebugInfo.GetColumnForPC(const APC: UInt32): UInt16;
var
  I: Integer;
begin
  Result := 0;
  for I := FLineMapCount - 1 downto 0 do
    if FLineMap[I].PC <= APC then
      Exit(FLineMap[I].Column);
end;

function TGocciaDebugInfo.GetLineMapEntry(const AIndex: Integer): TGocciaLineMapEntry;
begin
  Result := FLineMap[AIndex];
end;

function TGocciaDebugInfo.GetLocalInfo(const AIndex: Integer): TGocciaLocalInfo;
begin
  Result := FLocals[AIndex];
end;

end.
