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
    // The line of the last lookup: parsing asks for positions mostly in source
    // order, so the next one is usually on the same line or the one after.
    FLastLine: Integer;
    function GetSource: string;
  public
    constructor Create(const ASource: string);
    procedure PositionAtOffset(const AOffset: Integer; out ALine,
      AColumn: Integer);
    function OffsetAtPosition(const ALine, AColumn: Integer): Integer;
  end;

  // A node's source text (Function.prototype.toString), kept as a range of
  // the parse's source until something reads it. A copy per function cost
  // memory in proportion to the file size times its nesting depth: a bundle's
  // outer function holds nearly the whole file, and every function inside
  // copied its own text again.
  TGocciaLazySourceText = record
  private
    FText: string;      // text set explicitly
    FSource: string;    // the whole source (shared, not copied) for a range
    FStart, FLength: Integer;
  public
    function Get: string;
    procedure SetText(const AText: string);
    procedure SetRange(const ASource: string; const AStart, ALength: Integer);
    function IsEmpty: Boolean;
  end;

  TGocciaSourceSpan = record
  private
    FStartOffset: Integer;
    FEndOffset: Integer;
    // Not reference counted: a span is copied in and out of every node and
    // token, and a managed field makes each copy an RTTI-driven initialize,
    // finalize and implicit exception frame. The lexer keeps the coordinates
    // alive for its tokens, and a node's arena for the tree
    // (TGocciaASTArena.KeepCoordinates), which outlives the lexer.
    FCoordinates: TGocciaSourceCoordinates;
    function GetStartLine: Integer;
    function GetStartColumn: Integer;
    function GetEndLine: Integer;
    function GetEndColumn: Integer;
    function GetSource: string;
  public
    class function FromOffsets(const AStartOffset,
      AEndOffset: Integer): TGocciaSourceSpan; static;
    class function InSource(const ACoordinates: IGocciaSourceCoordinates;
      const AStartOffset, AEndOffset: Integer): TGocciaSourceSpan; overload; static;
    class function InSource(const ACoordinates: TGocciaSourceCoordinates;
      const AStartOffset, AEndOffset: Integer): TGocciaSourceSpan; overload; static; inline;
    class function Empty: TGocciaSourceSpan; static;
    function InSameSource(const AStartOffset,
      AEndOffset: Integer): TGocciaSourceSpan;
    function Cover(const AOther: TGocciaSourceSpan): TGocciaSourceSpan;
    function Length: Integer;
    { The exact source text this span covers, or '' when the span carries no
      coordinates (synthetic nodes, shim-generated AST). Used by runtime
      diagnostics that name an expression the way the author wrote it. }
    function Text: string;
    function OffsetAtPosition(const ALine, AColumn: Integer): Integer;
    property StartOffset: Integer read FStartOffset;
    property EndOffset: Integer read FEndOffset;
    property StartLine: Integer read GetStartLine;
    property StartColumn: Integer read GetStartColumn;
    property EndLine: Integer read GetEndLine;
    property EndColumn: Integer read GetEndColumn;
    property Source: string read GetSource;
    { The coordinates the offsets resolve against; nil for a span made from
      offsets alone. Not reference counted (see FCoordinates). }
    property Coordinates: TGocciaSourceCoordinates read FCoordinates;
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
  if Assigned(ACoordinates) then
    Result.FCoordinates := ACoordinates as TGocciaSourceCoordinates
  else
    Result.FCoordinates := nil;
  Result.FStartOffset := AStartOffset;
  Result.FEndOffset := AEndOffset;
end;

class function TGocciaSourceSpan.InSource(
  const ACoordinates: TGocciaSourceCoordinates; const AStartOffset,
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

function TGocciaSourceSpan.Text: string;
var
  SourceText: string;
  Count, SourceLength: Integer;
begin
  Result := '';
  if not Assigned(FCoordinates) then
    Exit;
  SourceText := FCoordinates.GetSource;
  SourceLength := System.Length(SourceText);
  if (FStartOffset < 0) or (FStartOffset >= SourceLength) or
     (FEndOffset <= FStartOffset) then
    Exit;
  Count := FEndOffset - FStartOffset;
  if FStartOffset + Count > SourceLength then
    Count := SourceLength - FStartOffset;
  Result := Copy(SourceText, FStartOffset + 1, Count);
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
    Result := FCoordinates.GetSource
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

// Wrapping arithmetic (hash, SWAR): no overflow or range checks, which a
// development build turns on.
{$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
constructor TGocciaSourceCoordinates.Create(const ASource: string);
var
  Index, Len, LineCount: Integer;
  P: PChar;
  C: Char;
begin
  inherited Create;
  FSource := ASource;
  SetLength(FLineOffsets, 16);
  FLineOffsets[0] := 0;
  LineCount := 1;
  // One pass over the source: most characters are above #13 and below
  // U+2028, so one comparison pair rejects them.
  P := PChar(FSource);
  Len := Length(FSource);
  Index := 0;
  while Index < Len do
  begin
    // Four characters at a time while all are ASCII above #13: subtracting
    // 14 from each sets a word's top bit only if it was below 14, so a zero
    // result rules out every terminator (a borrow can only add false alarms).
    while (Index + 4 <= Len) and
      (PQWord(@P[Index])^ and QWord($FF80FF80FF80FF80) = 0) and
      ((PQWord(@P[Index])^ - QWord($000E000E000E000E)) and QWord($8000800080008000) = 0) do
      Inc(Index, 4);
    if Index >= Len then
      Break;
    C := P[Index];
    Inc(Index);
    if (Ord(C) > 13) and (Ord(C) < $2028) then
      Continue;
    if C = #13 then
    begin
      if (Index < Len) and (P[Index] = #10) then
        Inc(Index);
    end
    else if (C <> #10) and (C <> Char($2028)) and (C <> Char($2029)) then
      Continue;

    if LineCount = Length(FLineOffsets) then
      SetLength(FLineOffsets, Length(FLineOffsets) * 2);
    FLineOffsets[LineCount] := Index;
    Inc(LineCount);
  end;
  SetLength(FLineOffsets, LineCount);
end;
{$POP}

{ TGocciaLazySourceText }

function TGocciaLazySourceText.Get: string;
begin
  if FLength > 0 then
    Result := Copy(FSource, FStart + 1, FLength)
  else
    Result := FText;
end;

procedure TGocciaLazySourceText.SetText(const AText: string);
begin
  FText := AText;
  FSource := '';
  FStart := 0;
  FLength := 0;
end;

procedure TGocciaLazySourceText.SetRange(const ASource: string;
  const AStart, ALength: Integer);
begin
  FText := '';
  if ALength <= 0 then
  begin
    FSource := '';
    FStart := 0;
    FLength := 0;
    Exit;
  end;
  FSource := ASource;
  FStart := AStart;
  FLength := ALength;
end;

function TGocciaLazySourceText.IsEmpty: Boolean;
begin
  Result := (FLength <= 0) and (FText = '');
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

  // The last lookup's line, or the next one, before the binary search. The
  // cached index is only a hint: it is checked against the offsets each time.
  First := FLastLine;
  if (First >= 0) and (First <= High(FLineOffsets)) and
     (FLineOffsets[First] <= Offset) then
  begin
    if (First = High(FLineOffsets)) or (FLineOffsets[First + 1] > Offset) then
    begin
      ALine := First + 1;
      AColumn := Offset - FLineOffsets[First] + 1;
      Exit;
    end;
    if (First + 1 < High(FLineOffsets)) and (FLineOffsets[First + 2] > Offset) then
    begin
      FLastLine := First + 1;
      ALine := First + 2;
      AColumn := Offset - FLineOffsets[First + 1] + 1;
      Exit;
    end;
    if First + 1 = High(FLineOffsets) then
    begin
      FLastLine := First + 1;
      ALine := First + 2;
      AColumn := Offset - FLineOffsets[First + 1] + 1;
      Exit;
    end;
  end;
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

  FLastLine := First;
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
