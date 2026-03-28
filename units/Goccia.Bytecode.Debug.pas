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
    Slot: UInt8;
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
  public
    constructor Create(const ASourceFile: string);

    procedure AddLineMapping(const APC: UInt32; const ALine: UInt32;
      const AColumn: UInt16);
    procedure AddLocal(const AName: string; const ASlot: UInt8;
      const AStartPC, AEndPC: UInt32);

    function GetLineForPC(const APC: UInt32): UInt32;
    function GetColumnForPC(const APC: UInt32): UInt16;
    function GetLocalName(const ASlot: UInt8; const APC: UInt32): string;

    function GetLineMapEntry(const AIndex: Integer): TGocciaLineMapEntry;
    function GetLocalInfo(const AIndex: Integer): TGocciaLocalInfo;

    property SourceFile: string read FSourceFile;
    property LineMapCount: Integer read FLineMapCount;
    property LocalCount: Integer read FLocalCount;
  end;

implementation

constructor TGocciaDebugInfo.Create(const ASourceFile: string);
begin
  inherited Create;
  FSourceFile := ASourceFile;
  FLineMapCount := 0;
  FLocalCount := 0;
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

procedure TGocciaDebugInfo.AddLocal(const AName: string; const ASlot: UInt8;
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

function TGocciaDebugInfo.GetLocalName(const ASlot: UInt8;
  const APC: UInt32): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FLocalCount - 1 do
    if (FLocals[I].Slot = ASlot) and (APC >= FLocals[I].StartPC) and
       (APC <= FLocals[I].EndPC) then
      Exit(FLocals[I].Name);
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
