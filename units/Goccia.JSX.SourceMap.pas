unit Goccia.JSX.SourceMap;

{$I Goccia.inc}

interface

type
  TGocciaSourceMapping = record
    OutputLine: Integer;
    OutputColumn: Integer;
    SourceLine: Integer;
    SourceColumn: Integer;
  end;

  TGocciaSourceMap = class
  private
    FMappings: array of TGocciaSourceMapping;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow;
    function FindLineStart(const AOutputLine: Integer): Integer;
  public
    constructor Create;
    procedure AddMapping(const AOutputLine, AOutputColumn, ASourceLine, ASourceColumn: Integer);
    function Translate(const ALine, AColumn: Integer;
      out ASourceLine, ASourceColumn: Integer): Boolean;
    property Count: Integer read FCount;
  end;

implementation

constructor TGocciaSourceMap.Create;
begin
  FCount := 0;
  FCapacity := 64;
  SetLength(FMappings, FCapacity);
end;

procedure TGocciaSourceMap.Grow;
begin
  FCapacity := FCapacity * 2;
  SetLength(FMappings, FCapacity);
end;

procedure TGocciaSourceMap.AddMapping(const AOutputLine, AOutputColumn, ASourceLine, ASourceColumn: Integer);
begin
  if FCount >= FCapacity then
    Grow;
  FMappings[FCount].OutputLine := AOutputLine;
  FMappings[FCount].OutputColumn := AOutputColumn;
  FMappings[FCount].SourceLine := ASourceLine;
  FMappings[FCount].SourceColumn := ASourceColumn;
  Inc(FCount);
end;

function TGocciaSourceMap.FindLineStart(const AOutputLine: Integer): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := 0;
  Hi := FCount - 1;
  Result := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if FMappings[Mid].OutputLine < AOutputLine then
      Lo := Mid + 1
    else if FMappings[Mid].OutputLine > AOutputLine then
      Hi := Mid - 1
    else
    begin
      Result := Mid;
      Hi := Mid - 1;
    end;
  end;
end;

function TGocciaSourceMap.Translate(const ALine, AColumn: Integer;
  out ASourceLine, ASourceColumn: Integer): Boolean;
var
  I, BestIndex, LineStart: Integer;
begin
  Result := False;
  if FCount = 0 then
    Exit;

  LineStart := FindLineStart(ALine);

  if LineStart < 0 then
  begin
    BestIndex := -1;
    for I := FCount - 1 downto 0 do
    begin
      if FMappings[I].OutputLine <= ALine then
      begin
        BestIndex := I;
        Break;
      end;
    end;
    if BestIndex < 0 then
      Exit;
    ASourceLine := FMappings[BestIndex].SourceLine + (ALine - FMappings[BestIndex].OutputLine);
    ASourceColumn := AColumn;
    Result := True;
    Exit;
  end;

  BestIndex := LineStart;
  I := LineStart;
  while (I < FCount) and (FMappings[I].OutputLine = ALine) do
  begin
    if FMappings[I].OutputColumn <= AColumn then
      BestIndex := I
    else
      Break;
    Inc(I);
  end;

  ASourceLine := FMappings[BestIndex].SourceLine;
  ASourceColumn := FMappings[BestIndex].SourceColumn + (AColumn - FMappings[BestIndex].OutputColumn);
  Result := True;
end;

end.
