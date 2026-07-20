unit Goccia.SourceSpan;

{$I Goccia.inc}

interface

type
  TGocciaSourceLineOffsets = array of Integer;

  IGocciaSourceCoordinates = interface
    ['{48D72D03-44D3-4B22-A4D9-D75660E5C8C9}']
    function GetSource: string;
    procedure PositionAtOffset(const AOffset: Integer; out ALine,
      AColumn: Integer);
    function OffsetAtPosition(const ALine, AColumn: Integer): Integer;
    property Source: string read GetSource;
  end;

  TGocciaSourceCoordinates = class(TInterfacedObject, IGocciaSourceCoordinates)
  private
    FSource: string;
    FLineOffsets: TGocciaSourceLineOffsets;
    function GetSource: string;
  public
    constructor Create(const ASource: string);
    procedure PositionAtOffset(const AOffset: Integer; out ALine,
      AColumn: Integer);
    function OffsetAtPosition(const ALine, AColumn: Integer): Integer;
  end;

  TGocciaSourceSpan = record
  private
    FStartOffset: Integer;
    FEndOffset: Integer;
    FCoordinates: IGocciaSourceCoordinates;
    function GetStartLine: Integer;
    function GetStartColumn: Integer;
    function GetEndLine: Integer;
    function GetEndColumn: Integer;
    function GetSource: string;
  public
    class function FromOffsets(const AStartOffset,
      AEndOffset: Integer): TGocciaSourceSpan; static;
    class function InSource(const ACoordinates: IGocciaSourceCoordinates;
      const AStartOffset, AEndOffset: Integer): TGocciaSourceSpan; static;
    class function Empty: TGocciaSourceSpan; static;
    function InSameSource(const AStartOffset,
      AEndOffset: Integer): TGocciaSourceSpan;
    function Cover(const AOther: TGocciaSourceSpan): TGocciaSourceSpan;
    function Length: Integer;
    function OffsetAtPosition(const ALine, AColumn: Integer): Integer;
    property StartOffset: Integer read FStartOffset;
    property EndOffset: Integer read FEndOffset;
    property StartLine: Integer read GetStartLine;
    property StartColumn: Integer read GetStartColumn;
    property EndLine: Integer read GetEndLine;
    property EndColumn: Integer read GetEndColumn;
    property Source: string read GetSource;
  end;

implementation

class function TGocciaSourceSpan.FromOffsets(const AStartOffset,
  AEndOffset: Integer): TGocciaSourceSpan;
begin
  Result.FCoordinates := nil;
  Result.FStartOffset := AStartOffset;
  Result.FEndOffset := AEndOffset;
end;

class function TGocciaSourceSpan.InSource(
  const ACoordinates: IGocciaSourceCoordinates; const AStartOffset,
  AEndOffset: Integer): TGocciaSourceSpan;
begin
  Result.FCoordinates := ACoordinates;
  Result.FStartOffset := AStartOffset;
  Result.FEndOffset := AEndOffset;
end;

class function TGocciaSourceSpan.Empty: TGocciaSourceSpan;
begin
  Result := FromOffsets(0, 0);
end;

function TGocciaSourceSpan.InSameSource(const AStartOffset,
  AEndOffset: Integer): TGocciaSourceSpan;
begin
  Result := InSource(FCoordinates, AStartOffset, AEndOffset);
end;

function TGocciaSourceSpan.Cover(
  const AOther: TGocciaSourceSpan): TGocciaSourceSpan;
var
  StartOffset, EndOffset: Integer;
begin
  StartOffset := FStartOffset;
  if AOther.FStartOffset < StartOffset then
    StartOffset := AOther.FStartOffset;
  EndOffset := FEndOffset;
  if AOther.FEndOffset > EndOffset then
    EndOffset := AOther.FEndOffset;
  Result := InSource(FCoordinates, StartOffset, EndOffset);
end;

function TGocciaSourceSpan.Length: Integer;
begin
  Result := FEndOffset - FStartOffset;
end;

function TGocciaSourceSpan.GetStartLine: Integer;
var
  Column: Integer;
begin
  if not Assigned(FCoordinates) then
    Exit(0);
  FCoordinates.PositionAtOffset(FStartOffset, Result, Column);
end;

function TGocciaSourceSpan.GetStartColumn: Integer;
var
  Line: Integer;
begin
  if not Assigned(FCoordinates) then
    Exit(0);
  FCoordinates.PositionAtOffset(FStartOffset, Line, Result);
end;

function TGocciaSourceSpan.GetEndLine: Integer;
var
  Column, Offset: Integer;
begin
  if not Assigned(FCoordinates) then
    Exit(0);
  Offset := FEndOffset;
  if Offset > FStartOffset then
    Dec(Offset);
  FCoordinates.PositionAtOffset(Offset, Result, Column);
end;

function TGocciaSourceSpan.GetEndColumn: Integer;
var
  Line, Offset: Integer;
begin
  if not Assigned(FCoordinates) then
    Exit(0);
  Offset := FEndOffset;
  if Offset > FStartOffset then
    Dec(Offset);
  FCoordinates.PositionAtOffset(Offset, Line, Result);
end;

function TGocciaSourceSpan.GetSource: string;
begin
  if Assigned(FCoordinates) then
    Result := FCoordinates.Source
  else
    Result := '';
end;

function TGocciaSourceSpan.OffsetAtPosition(const ALine,
  AColumn: Integer): Integer;
begin
  if not Assigned(FCoordinates) then
    Exit(-1);
  Result := FCoordinates.OffsetAtPosition(ALine, AColumn);
end;

constructor TGocciaSourceCoordinates.Create(const ASource: string);
var
  Index, LineCount: Integer;
begin
  inherited Create;
  FSource := ASource;
  SetLength(FLineOffsets, 16);
  FLineOffsets[0] := 0;
  LineCount := 1;
  Index := 1;
  while Index <= Length(FSource) do
  begin
    if FSource[Index] = #13 then
    begin
      Inc(Index);
      if (Index <= Length(FSource)) and (FSource[Index] = #10) then
        Inc(Index);
    end
    else if (FSource[Index] = #10) or (FSource[Index] = Char($2028)) or
      (FSource[Index] = Char($2029)) then
      Inc(Index)
    else
    begin
      Inc(Index);
      Continue;
    end;

    if LineCount = Length(FLineOffsets) then
      SetLength(FLineOffsets, Length(FLineOffsets) * 2);
    FLineOffsets[LineCount] := Index - 1;
    Inc(LineCount);
  end;
  SetLength(FLineOffsets, LineCount);
end;

function TGocciaSourceCoordinates.GetSource: string;
begin
  Result := FSource;
end;

procedure TGocciaSourceCoordinates.PositionAtOffset(const AOffset: Integer;
  out ALine, AColumn: Integer);
var
  First, Last, Middle, Offset: Integer;
begin
  Offset := AOffset;
  if Offset < 0 then
    Offset := 0
  else if Offset > Length(FSource) then
    Offset := Length(FSource);

  First := 0;
  Last := High(FLineOffsets);
  while First < Last do
  begin
    Middle := First + (Last - First + 1) div 2;
    if FLineOffsets[Middle] <= Offset then
      First := Middle
    else
      Last := Middle - 1;
  end;

  ALine := First + 1;
  AColumn := Offset - FLineOffsets[First] + 1;
end;

function TGocciaSourceCoordinates.OffsetAtPosition(const ALine,
  AColumn: Integer): Integer;
begin
  if (ALine < 1) or (ALine > Length(FLineOffsets)) or (AColumn < 1) then
    Exit(-1);

  Result := FLineOffsets[ALine - 1] + AColumn - 1;
  if Result > Length(FSource) then
    Result := Length(FSource);
end;

end.
