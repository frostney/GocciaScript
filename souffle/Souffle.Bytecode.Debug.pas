unit Souffle.Bytecode.Debug;

{$I Souffle.inc}

interface

type
  TSouffleLineMapEntry = record
    PC: UInt32;
    Line: UInt32;
    Column: UInt16;
  end;

  TSouffleLocalInfo = record
    Name: string;
    Slot: UInt8;
    StartPC: UInt32;
    EndPC: UInt32;
  end;

  TSouffleDebugInfo = class
  private
    FSourceFile: string;
    FLineMap: array of TSouffleLineMapEntry;
    FLineMapCount: Integer;
    FLocals: array of TSouffleLocalInfo;
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

    function GetLineMapEntry(const AIndex: Integer): TSouffleLineMapEntry;
    function GetLocalInfo(const AIndex: Integer): TSouffleLocalInfo;

    property SourceFile: string read FSourceFile;
    property LineMapCount: Integer read FLineMapCount;
    property LocalCount: Integer read FLocalCount;
  end;

implementation

{ TSouffleDebugInfo }

constructor TSouffleDebugInfo.Create(const ASourceFile: string);
begin
  inherited Create;
  FSourceFile := ASourceFile;
  FLineMapCount := 0;
  FLocalCount := 0;
end;

procedure TSouffleDebugInfo.AddLineMapping(const APC: UInt32;
  const ALine: UInt32; const AColumn: UInt16);
begin
  if FLineMapCount >= Length(FLineMap) then
    SetLength(FLineMap, FLineMapCount * 2 + 8);
  FLineMap[FLineMapCount].PC := APC;
  FLineMap[FLineMapCount].Line := ALine;
  FLineMap[FLineMapCount].Column := AColumn;
  Inc(FLineMapCount);
end;

procedure TSouffleDebugInfo.AddLocal(const AName: string; const ASlot: UInt8;
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

function TSouffleDebugInfo.GetLineForPC(const APC: UInt32): UInt32;
var
  I: Integer;
begin
  Result := 0;
  for I := FLineMapCount - 1 downto 0 do
    if FLineMap[I].PC <= APC then
      Exit(FLineMap[I].Line);
end;

function TSouffleDebugInfo.GetColumnForPC(const APC: UInt32): UInt16;
var
  I: Integer;
begin
  Result := 0;
  for I := FLineMapCount - 1 downto 0 do
    if FLineMap[I].PC <= APC then
      Exit(FLineMap[I].Column);
end;

function TSouffleDebugInfo.GetLocalName(const ASlot: UInt8;
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

function TSouffleDebugInfo.GetLineMapEntry(const AIndex: Integer): TSouffleLineMapEntry;
begin
  Result := FLineMap[AIndex];
end;

function TSouffleDebugInfo.GetLocalInfo(const AIndex: Integer): TSouffleLocalInfo;
begin
  Result := FLocals[AIndex];
end;

end.
