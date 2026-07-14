unit Goccia.TestRunner.SnapshotHost;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Builtins.Testing.Snapshots;

type
  TGocciaTestRunnerSnapshotHost = class(TInterfacedObject,
    IGocciaSnapshotHost)
  private
    FSourcePath: string;
    FBackingSourcePath: string;
    FSnapshotPath: string;
    FInlineEdits: TList<TGocciaInlineSnapshotEdit>;

    procedure WriteUTF8Text(const APath: string; const AContent: UTF8String);
  public
    constructor Create(const ASourcePath: string);
    destructor Destroy; override;

    function HasPersistentSource: Boolean;
    function ReadSnapshotFile(out AContent: string): Boolean;
    procedure WriteSnapshotFile(const AContent: string);
    procedure DeleteSnapshotFile;
    procedure QueueInlineSnapshot(const AEdit: TGocciaInlineSnapshotEdit);
    procedure FlushInlineSnapshots;

    property SnapshotPath: string read FSnapshotPath;
  end;

{ Inline edits are process-wide because several test engines can execute the
  same imported source or different virtual sections of one multifile source.
  Engines only enqueue edits; the runner applies the combined descending edit
  set after every engine has finished reading the original source. }
procedure ResetPendingInlineSnapshots;
procedure FlushPendingInlineSnapshots;

implementation

uses
  Classes,
  Generics.Defaults,
  Math,
  SysUtils,

  TextSemantics,

  Goccia.ScriptLoader.Input,
  Goccia.TextFiles;

type
  TSourceRange = record
    StartOffset: Integer;
    EndOffset: Integer;
  end;

var
  InlineSnapshotWriteLock: TRTLCriticalSection;
  PendingInlineEdits: TList<TGocciaInlineSnapshotEdit>;
  PendingInlineError: string;

procedure WriteUTF8TextFile(const APath: string; const AContent: UTF8String);
var
  Stream: TFileStream;
begin
  ForceDirectories(ExtractFileDir(APath));
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(AContent) > 0 then
      Stream.WriteBuffer(AContent[1], Length(AContent));
  finally
    Stream.Free;
  end;
end;

function NormalizeNewlines(const AValue: string): string;
begin
  Result := StringReplace(AValue, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function TrimSnapshot(const AValue: string): string;
begin
  Result := Trim(NormalizeNewlines(AValue));
end;

function LineTerminatorLengthAt(const ASource: string;
  const AIndex: Integer): Integer;
var
  ByteLength: Integer;
  CodePoint: Cardinal;
begin
  Result := 0;
  if (AIndex < 1) or (AIndex > Length(ASource)) then
    Exit;
  if ASource[AIndex] = #13 then
  begin
    Result := 1;
    if (AIndex < Length(ASource)) and (ASource[AIndex + 1] = #10) then
      Result := 2;
    Exit;
  end;
  if ASource[AIndex] = #10 then
    Exit(1);
  if TryReadUTF8CodePoint(ASource, AIndex, CodePoint, ByteLength) and
     ((CodePoint = $2028) or (CodePoint = $2029)) then
    Result := ByteLength;
end;

procedure SkipECMAScriptWhitespace(const ASource: string;
  var AIndex: Integer);
var
  ByteLength: Integer;
  CodePoint: Cardinal;
begin
  while AIndex <= Length(ASource) do
  begin
    if not TryReadUTF8CodePoint(ASource, AIndex, CodePoint,
       ByteLength) then
      Exit;
    if not IsECMAScriptWhitespaceCodePoint(CodePoint) then
      Exit;
    Inc(AIndex, ByteLength);
  end;
end;

procedure SkipECMAScriptWhitespaceBackward(const ASource: string;
  var AIndex: Integer);
var
  ByteLength, StartIndex: Integer;
  CodePoint: Cardinal;
begin
  while AIndex >= 1 do
  begin
    StartIndex := AIndex;
    while (StartIndex > 1) and
          ((Ord(ASource[StartIndex]) and $C0) = $80) do
      Dec(StartIndex);
    if not TryReadUTF8CodePoint(ASource, StartIndex, CodePoint,
       ByteLength) or (StartIndex + ByteLength - 1 <> AIndex) or
       not IsECMAScriptWhitespaceCodePoint(CodePoint) then
      Exit;
    AIndex := StartIndex - 1;
  end;
end;

function LineColumnToOffset(const ASource: string; const ALine,
  AColumn: Integer): Integer;
var
  ByteLength, Column, Index, Line: Integer;
  CodePoint: Cardinal;
begin
  if (ALine < 1) or (AColumn < 1) then
    Exit(0);

  Index := 1;
  Line := 1;
  while (Index <= Length(ASource)) and (Line < ALine) do
  begin
    if ASource[Index] = #13 then
    begin
      Inc(Line);
      Inc(Index);
      if (Index <= Length(ASource)) and (ASource[Index] = #10) then
        Inc(Index);
    end
    else if ASource[Index] = #10 then
    begin
      Inc(Line);
      Inc(Index);
    end
    else if TryReadUTF8CodePoint(ASource, Index, CodePoint, ByteLength) and
            ((CodePoint = $2028) or (CodePoint = $2029)) then
    begin
      Inc(Line);
      Inc(Index, ByteLength);
    end
    else
      Inc(Index);
  end;
  if Line <> ALine then
    Exit(0);

  Column := 1;
  while (Index <= Length(ASource)) and (Column < AColumn) and
        not (ASource[Index] in [#10, #13]) do
  begin
    if TryReadUTF8CodePoint(ASource, Index, CodePoint, ByteLength) and
       ((CodePoint = $2028) or (CodePoint = $2029)) then
      Break;
    Inc(Index);
    Inc(Column);
  end;
  if Column <> AColumn then
    Exit(0);
  Result := Index;
end;

procedure SkipStringLiteral(const ASource: string; var AIndex: Integer;
  const AQuote: Char);
begin
  Inc(AIndex);
  while AIndex <= Length(ASource) do
  begin
    if ASource[AIndex] = '\' then
      Inc(AIndex, 2)
    else if ASource[AIndex] = AQuote then
    begin
      Inc(AIndex);
      Exit;
    end
    else
      Inc(AIndex);
  end;
end;

procedure SkipLineComment(const ASource: string; var AIndex: Integer);
begin
  Inc(AIndex, 2);
  while (AIndex <= Length(ASource)) and
        (LineTerminatorLengthAt(ASource, AIndex) = 0) do
    Inc(AIndex);
end;

procedure SkipBlockComment(const ASource: string; var AIndex: Integer);
begin
  Inc(AIndex, 2);
  while AIndex < Length(ASource) do
  begin
    if (ASource[AIndex] = '*') and (ASource[AIndex + 1] = '/') then
    begin
      Inc(AIndex, 2);
      Exit;
    end;
    Inc(AIndex);
  end;
  AIndex := Length(ASource) + 1;
end;

function PreviousSignificantChar(const ASource: string;
  const AIndex: Integer): Char;
var
  Index: Integer;
begin
  Index := AIndex - 1;
  SkipECMAScriptWhitespaceBackward(ASource, Index);
  if Index < 1 then
    Exit(#0);
  Result := ASource[Index];
end;

function StartsRegExpLiteral(const ASource: string;
  const AIndex: Integer): Boolean;
var
  Index, TokenEnd: Integer;
  Previous: Char;
  Token: string;
begin
  Previous := PreviousSignificantChar(ASource, AIndex);
  Result := (Previous = #0) or
    (Previous in ['(', '[', '{', ',', ':', ';', '=', '!', '&', '|', '?',
      '+', '-', '*', '%', '^', '~', '<', '>']);
  if Result then
    Exit;

  Index := AIndex - 1;
  SkipECMAScriptWhitespaceBackward(ASource, Index);
  TokenEnd := Index;
  while (Index >= 1) and
        (ASource[Index] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) do
    Dec(Index);
  Token := Copy(ASource, Index + 1, TokenEnd - Index);
  Result := (Token = 'return') or (Token = 'throw') or
    (Token = 'case') or (Token = 'delete') or (Token = 'void') or
    (Token = 'typeof') or (Token = 'instanceof') or (Token = 'in') or
    (Token = 'of') or (Token = 'yield') or (Token = 'await') or
    (Token = 'new') or (Token = 'else') or (Token = 'do');
end;

procedure SkipRegExpLiteral(const ASource: string; var AIndex: Integer);
var
  InCharacterClass: Boolean;
begin
  Inc(AIndex);
  InCharacterClass := False;
  while AIndex <= Length(ASource) do
  begin
    if ASource[AIndex] = '\' then
      Inc(AIndex, 2)
    else if ASource[AIndex] = '[' then
    begin
      InCharacterClass := True;
      Inc(AIndex);
    end
    else if ASource[AIndex] = ']' then
    begin
      InCharacterClass := False;
      Inc(AIndex);
    end
    else if (ASource[AIndex] = '/') and not InCharacterClass then
    begin
      Inc(AIndex);
      while (AIndex <= Length(ASource)) and
            (ASource[AIndex] in ['a'..'z', 'A'..'Z']) do
        Inc(AIndex);
      Exit;
    end
    else
      Inc(AIndex);
  end;
end;

procedure SkipTemplateLiteral(const ASource: string; var AIndex: Integer);
  forward;

procedure SkipTemplateExpression(const ASource: string; var AIndex: Integer);
var
  BraceDepth: Integer;
begin
  BraceDepth := 1;
  while (AIndex <= Length(ASource)) and (BraceDepth > 0) do
  begin
    case ASource[AIndex] of
      '''', '"': SkipStringLiteral(ASource, AIndex, ASource[AIndex]);
      '`': SkipTemplateLiteral(ASource, AIndex);
      '/':
        if (AIndex < Length(ASource)) and (ASource[AIndex + 1] = '/') then
          SkipLineComment(ASource, AIndex)
        else if (AIndex < Length(ASource)) and
                (ASource[AIndex + 1] = '*') then
          SkipBlockComment(ASource, AIndex)
        else if StartsRegExpLiteral(ASource, AIndex) then
          SkipRegExpLiteral(ASource, AIndex)
        else
          Inc(AIndex);
      '{':
        begin
          Inc(BraceDepth);
          Inc(AIndex);
        end;
      '}':
        begin
          Dec(BraceDepth);
          Inc(AIndex);
        end;
    else
      Inc(AIndex);
    end;
  end;
end;

procedure SkipTemplateLiteral(const ASource: string; var AIndex: Integer);
begin
  Inc(AIndex);
  while AIndex <= Length(ASource) do
  begin
    if ASource[AIndex] = '\' then
      Inc(AIndex, 2)
    else if ASource[AIndex] = '`' then
    begin
      Inc(AIndex);
      Exit;
    end
    else if (ASource[AIndex] = '$') and (AIndex < Length(ASource)) and
            (ASource[AIndex + 1] = '{') then
    begin
      Inc(AIndex, 2);
      SkipTemplateExpression(ASource, AIndex);
    end
    else
      Inc(AIndex);
  end;
end;

procedure SkipTrivia(const ASource: string; var AIndex: Integer);
var
  PreviousIndex: Integer;
begin
  repeat
    PreviousIndex := AIndex;
    SkipECMAScriptWhitespace(ASource, AIndex);
    if (AIndex < Length(ASource)) and (ASource[AIndex] = '/') and
       (ASource[AIndex + 1] = '/') then
      SkipLineComment(ASource, AIndex)
    else if (AIndex < Length(ASource)) and (ASource[AIndex] = '/') and
            (ASource[AIndex + 1] = '*') then
      SkipBlockComment(ASource, AIndex);
  until PreviousIndex = AIndex;
end;

function FindCallArgumentRanges(const ASource: string;
  const AOpenParenOffset: Integer; out ARanges: TArray<TSourceRange>;
  out ACloseParenOffset: Integer): Boolean;
var
  BraceDepth, BracketDepth, Index, ParenDepth, RangeCount,
    RangeStart: Integer;

  procedure AddRange(const AStart, AEnd: Integer);
  begin
    SetLength(ARanges, RangeCount + 1);
    ARanges[RangeCount].StartOffset := AStart;
    ARanges[RangeCount].EndOffset := AEnd;
    Inc(RangeCount);
  end;

begin
  Result := False;
  SetLength(ARanges, 0);
  ACloseParenOffset := 0;
  if (AOpenParenOffset < 1) or (AOpenParenOffset > Length(ASource)) or
     (ASource[AOpenParenOffset] <> '(') then
    Exit;

  Index := AOpenParenOffset + 1;
  SkipTrivia(ASource, Index);
  if (Index <= Length(ASource)) and (ASource[Index] = ')') then
  begin
    ACloseParenOffset := Index;
    Exit(True);
  end;

  RangeStart := Index;
  RangeCount := 0;
  ParenDepth := 0;
  BracketDepth := 0;
  BraceDepth := 0;
  while Index <= Length(ASource) do
  begin
    case ASource[Index] of
      '''', '"': SkipStringLiteral(ASource, Index, ASource[Index]);
      '`': SkipTemplateLiteral(ASource, Index);
      '/':
        if (Index < Length(ASource)) and (ASource[Index + 1] = '/') then
          SkipLineComment(ASource, Index)
        else if (Index < Length(ASource)) and
                (ASource[Index + 1] = '*') then
          SkipBlockComment(ASource, Index)
        else if StartsRegExpLiteral(ASource, Index) then
          SkipRegExpLiteral(ASource, Index)
        else
          Inc(Index);
      '(':
        begin
          Inc(ParenDepth);
          Inc(Index);
        end;
      ')':
        if (ParenDepth = 0) and (BracketDepth = 0) and (BraceDepth = 0) then
        begin
          AddRange(RangeStart, Index);
          ACloseParenOffset := Index;
          Exit(True);
        end
        else
        begin
          Dec(ParenDepth);
          Inc(Index);
        end;
      '[':
        begin
          Inc(BracketDepth);
          Inc(Index);
        end;
      ']':
        begin
          Dec(BracketDepth);
          Inc(Index);
        end;
      '{':
        begin
          Inc(BraceDepth);
          Inc(Index);
        end;
      '}':
        begin
          Dec(BraceDepth);
          Inc(Index);
        end;
      ',':
        if (ParenDepth = 0) and (BracketDepth = 0) and (BraceDepth = 0) then
        begin
          AddRange(RangeStart, Index);
          Inc(Index);
          SkipTrivia(ASource, Index);
          RangeStart := Index;
        end
        else
          Inc(Index);
    else
      Inc(Index);
    end;
  end;
end;

function SourceLineIndent(const ASource: string;
  const AOffset: Integer): string;
var
  Index, LineStart, TerminatorLength: Integer;
begin
  LineStart := 1;
  Index := 1;
  while (Index < AOffset) and (Index <= Length(ASource)) do
  begin
    TerminatorLength := LineTerminatorLengthAt(ASource, Index);
    if TerminatorLength > 0 then
    begin
      Inc(Index, TerminatorLength);
      LineStart := Index;
    end
    else
      Inc(Index);
  end;
  Index := LineStart;
  while (Index <= Length(ASource)) and (ASource[Index] in [' ', #9]) do
    Inc(Index);
  Result := Copy(ASource, LineStart, Index - LineStart);
end;

function EscapeInlineSnapshot(const AValue: string): string;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '`', '\`', [rfReplaceAll]);
  Result := StringReplace(Result, '${', '\${', [rfReplaceAll]);
end;

function PrepareInlineSnapshot(const ASnapshot, ASource: string;
  const ACallOffset: Integer; const ALineBreak: string): string;
var
  I: Integer;
  Indent, IndentNext: string;
  Lines: TStringList;
  Snapshot: string;
begin
  Snapshot := EscapeInlineSnapshot(TrimSnapshot(ASnapshot));
  Lines := TStringList.Create;
  try
    Lines.Text := Snapshot;
    if Lines.Count <= 1 then
      Exit('`' + Snapshot + '`');

    Indent := SourceLineIndent(ASource, ACallOffset);
    if Pos(#9, Indent) > 0 then
      IndentNext := Indent + #9
    else
      IndentNext := Indent + '  ';

    Result := '`' + ALineBreak;
    for I := 0 to Lines.Count - 1 do
    begin
      if Lines[I] <> '' then
        Result := Result + IndentNext + Lines[I];
      if I < Lines.Count - 1 then
        Result := Result + ALineBreak;
    end;
    Result := Result + ALineBreak + Indent + '`';
  finally
    Lines.Free;
  end;
end;

function DetectLineBreak(const ASource: string): string;
begin
  if Pos(#13#10, ASource) > 0 then
    Result := #13#10
  else if Pos(#13, ASource) > 0 then
    Result := #13
  else
    Result := #10;
end;

function LocateInlineSnapshotCall(const ASource: string;
  const AReportedOffset: Integer): Integer;
const
  MATCHER_NAME = 'toMatchInlineSnapshot';
var
  CloseOffset, Index, LineEnd, LineStart, NameEnd, OpenOffset: Integer;
  Ranges: TArray<TSourceRange>;

  function FindForward(const AStart, AStop: Integer): Integer;
  var
    ScanIndex: Integer;
  begin
    ScanIndex := Max(AStart, 1);
    while (ScanIndex <= Length(ASource)) and (ScanIndex <= AStop) do
    begin
      case ASource[ScanIndex] of
        '''', '"': SkipStringLiteral(ASource, ScanIndex,
          ASource[ScanIndex]);
        '`': SkipTemplateLiteral(ASource, ScanIndex);
        '/':
          if (ScanIndex < Length(ASource)) and
             (ASource[ScanIndex + 1] = '/') then
            SkipLineComment(ASource, ScanIndex)
          else if (ScanIndex < Length(ASource)) and
                  (ASource[ScanIndex + 1] = '*') then
            SkipBlockComment(ASource, ScanIndex)
          else if StartsRegExpLiteral(ASource, ScanIndex) then
            SkipRegExpLiteral(ASource, ScanIndex)
          else
            Inc(ScanIndex);
      else
        if Copy(ASource, ScanIndex, Length(MATCHER_NAME)) = MATCHER_NAME then
        begin
          OpenOffset := ScanIndex + Length(MATCHER_NAME);
          SkipTrivia(ASource, OpenOffset);
          if (OpenOffset <= Length(ASource)) and
             (ASource[OpenOffset] = '(') and
             FindCallArgumentRanges(ASource, OpenOffset, Ranges,
               CloseOffset) then
            Exit(OpenOffset);
        end;
        Inc(ScanIndex);
      end;
    end;
    Result := 0;
  end;

  function FindOnReportedLine: Integer;
  var
    PreviousOpen, ScanIndex: Integer;
  begin
    PreviousOpen := 0;
    ScanIndex := LineStart;
    while (ScanIndex <= Length(ASource)) and (ScanIndex <= LineEnd) do
    begin
      case ASource[ScanIndex] of
        '''', '"': SkipStringLiteral(ASource, ScanIndex,
          ASource[ScanIndex]);
        '`': SkipTemplateLiteral(ASource, ScanIndex);
        '/':
          if (ScanIndex < Length(ASource)) and
             (ASource[ScanIndex + 1] = '/') then
            SkipLineComment(ASource, ScanIndex)
          else if (ScanIndex < Length(ASource)) and
                  (ASource[ScanIndex + 1] = '*') then
            SkipBlockComment(ASource, ScanIndex)
          else if StartsRegExpLiteral(ASource, ScanIndex) then
            SkipRegExpLiteral(ASource, ScanIndex)
          else
            Inc(ScanIndex);
      else
        if Copy(ASource, ScanIndex, Length(MATCHER_NAME)) = MATCHER_NAME then
        begin
          OpenOffset := ScanIndex + Length(MATCHER_NAME);
          SkipTrivia(ASource, OpenOffset);
          if (OpenOffset <= Length(ASource)) and
             (ASource[OpenOffset] = '(') and
             FindCallArgumentRanges(ASource, OpenOffset, Ranges,
               CloseOffset) then
          begin
            if AReportedOffset <= CloseOffset then
              Exit(OpenOffset);
            PreviousOpen := OpenOffset;
          end;
        end;
        Inc(ScanIndex);
      end;
    end;
    Result := PreviousOpen;
  end;

begin
  LineStart := 1;
  LineEnd := 1;
  while (LineEnd < AReportedOffset) and (LineEnd <= Length(ASource)) do
  begin
    NameEnd := LineTerminatorLengthAt(ASource, LineEnd);
    if NameEnd > 0 then
    begin
      Inc(LineEnd, NameEnd);
      LineStart := LineEnd;
    end
    else
      Inc(LineEnd);
  end;
  LineEnd := Min(Max(AReportedOffset, 1), Length(ASource));
  while (LineEnd <= Length(ASource)) and
        (LineTerminatorLengthAt(ASource, LineEnd) = 0) do
    Inc(LineEnd);

  { Normal interpreter and bytecode locations both identify the containing
    source line, even when bytecode points at the start of expect(...). }
  Result := FindOnReportedLine;
  if Result <> 0 then
    Exit;

  { A prior parallel edit can shift this call onto a later line. Matcher
    order remains stable, so the next lexically valid call is the target. }
  Result := FindForward(Max(AReportedOffset, 1), Length(ASource));
  if Result <> 0 then
    Exit;

  { Multiline calls can report an argument line after the matcher name. }
  Index := Min(AReportedOffset, Length(ASource));
  while Index >= 1 do
  begin
    if ASource[Index] = '(' then
    begin
      NameEnd := Index - 1;
      SkipECMAScriptWhitespaceBackward(ASource, NameEnd);
      if (NameEnd >= Length(MATCHER_NAME)) and
         (Copy(ASource, NameEnd - Length(MATCHER_NAME) + 1,
           Length(MATCHER_NAME)) = MATCHER_NAME) and
         FindCallArgumentRanges(ASource, Index, Ranges, CloseOffset) and
         (CloseOffset >= AReportedOffset) then
      begin
        Result := Index;
        Exit;
      end;
    end;
    Dec(Index);
  end;
  Result := 0;
end;

function ApplyInlineEdit(const ASource: string;
  const AEdit: TGocciaInlineSnapshotEdit): string;
var
  CallOffset, CloseOffset: Integer;
  Prepared: string;
  Ranges: TArray<TSourceRange>;
  ReplaceEnd, ReplaceStart: Integer;
begin
  CallOffset := LineColumnToOffset(ASource, AEdit.Line, AEdit.Column);
  if CallOffset <> 0 then
    CallOffset := LocateInlineSnapshotCall(ASource, CallOffset);
  if CallOffset = 0 then
    raise Exception.CreateFmt(
      'Cannot locate inline snapshot call at %d:%d',
      [AEdit.Line, AEdit.Column]);
  if not FindCallArgumentRanges(ASource, CallOffset, Ranges, CloseOffset) then
    raise Exception.CreateFmt(
      'Cannot parse inline snapshot call at %d:%d',
      [AEdit.Line, AEdit.Column]);

  Prepared := PrepareInlineSnapshot(AEdit.Snapshot, ASource, CallOffset,
    DetectLineBreak(ASource));
  if AEdit.SnapshotArgumentIndex < Length(Ranges) then
  begin
    ReplaceStart := Ranges[AEdit.SnapshotArgumentIndex].StartOffset;
    ReplaceEnd := Ranges[AEdit.SnapshotArgumentIndex].EndOffset;
    Result := Copy(ASource, 1, ReplaceStart - 1) + Prepared +
      Copy(ASource, ReplaceEnd, MaxInt);
  end
  else if AEdit.SnapshotArgumentIndex = 0 then
    Result := Copy(ASource, 1, CloseOffset - 1) + Prepared +
      Copy(ASource, CloseOffset, MaxInt)
  else if AEdit.SnapshotArgumentIndex = Length(Ranges) then
    Result := Copy(ASource, 1, CloseOffset - 1) + ', ' + Prepared +
      Copy(ASource, CloseOffset, MaxInt)
  else
    raise Exception.CreateFmt(
      'Cannot insert inline snapshot argument %d at %d:%d',
      [AEdit.SnapshotArgumentIndex, AEdit.Line, AEdit.Column]);
end;

function CompareInlineEdits(constref ALeft,
  ARight: TGocciaInlineSnapshotEdit): Integer;
begin
  Result := CompareText(ALeft.SourcePath, ARight.SourcePath);
  if Result <> 0 then
    Exit;
  if ALeft.Line <> ARight.Line then
    Exit(ARight.Line - ALeft.Line);
  Result := ARight.Column - ALeft.Column;
end;

function MultifileSectionLineOffset(const ASectionPath: string): Integer;
var
  BackingPath: string;
  HasContent: Boolean;
  I, PartCount, PartIndex, SegmentStart: Integer;
  Lines: TStringList;
begin
  Result := 0;
  if not IsMultifileSectionName(ASectionPath) then
    Exit;
  PartIndex := MultifilePartIndex(ASectionPath);
  BackingPath := MultifileOriginalPath(ASectionPath);
  Lines := CreateUTF8FileTextLines(ReadUTF8FileText(BackingPath));
  try
    PartCount := 0;
    SegmentStart := 0;
    HasContent := False;
    for I := 0 to Lines.Count do
    begin
      if (I = Lines.Count) or IsMultifileSeparator(Lines[I]) then
      begin
        if HasContent then
        begin
          Inc(PartCount);
          if PartCount = PartIndex then
            Exit(SegmentStart);
        end;
        SegmentStart := I + 1;
        HasContent := False;
      end
      else if Trim(Lines[I]) <> '' then
        HasContent := True;
    end;
  finally
    Lines.Free;
  end;
  raise Exception.Create('Cannot locate multifile snapshot section: ' +
    ASectionPath);
end;

{ TGocciaTestRunnerSnapshotHost }

constructor TGocciaTestRunnerSnapshotHost.Create(const ASourcePath: string);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  if FileExists(ASourcePath) then
    FBackingSourcePath := ASourcePath
  else
    FBackingSourcePath := MultifileOriginalPath(ASourcePath);
  FSnapshotPath := IncludeTrailingPathDelimiter(ExtractFileDir(ASourcePath)) +
    '__snapshots__' + PathDelim + ExtractFileName(ASourcePath) + '.snap';
  FInlineEdits := TList<TGocciaInlineSnapshotEdit>.Create;
end;

destructor TGocciaTestRunnerSnapshotHost.Destroy;
begin
  FInlineEdits.Free;
  inherited;
end;

function TGocciaTestRunnerSnapshotHost.HasPersistentSource: Boolean;
begin
  Result := FileExists(FBackingSourcePath);
end;

function TGocciaTestRunnerSnapshotHost.ReadSnapshotFile(
  out AContent: string): Boolean;
begin
  Result := FileExists(FSnapshotPath);
  if Result then
    AContent := string(ReadUTF8FileText(FSnapshotPath))
  else
    AContent := '';
end;

procedure TGocciaTestRunnerSnapshotHost.WriteUTF8Text(const APath: string;
  const AContent: UTF8String);
begin
  WriteUTF8TextFile(APath, AContent);
end;

procedure TGocciaTestRunnerSnapshotHost.WriteSnapshotFile(
  const AContent: string);
begin
  WriteUTF8Text(FSnapshotPath, UTF8String(AContent));
end;

procedure TGocciaTestRunnerSnapshotHost.DeleteSnapshotFile;
begin
  if FileExists(FSnapshotPath) and not SysUtils.DeleteFile(FSnapshotPath) then
    raise Exception.Create('Cannot delete snapshot file: ' + FSnapshotPath);
end;

procedure TGocciaTestRunnerSnapshotHost.QueueInlineSnapshot(
  const AEdit: TGocciaInlineSnapshotEdit);
var
  Existing, NormalizedEdit: TGocciaInlineSnapshotEdit;
begin
  NormalizedEdit := AEdit;
  if NormalizedEdit.SourcePath = '' then
    NormalizedEdit.SourcePath := FSourcePath;
  if not FileExists(NormalizedEdit.SourcePath) and
     IsMultifileSectionName(NormalizedEdit.SourcePath) then
  begin
    Inc(NormalizedEdit.Line,
      MultifileSectionLineOffset(NormalizedEdit.SourcePath));
    NormalizedEdit.SourcePath := MultifileOriginalPath(
      NormalizedEdit.SourcePath);
  end;
  for Existing in FInlineEdits do
    if (Existing.SourcePath = NormalizedEdit.SourcePath) and
       (Existing.Line = NormalizedEdit.Line) and
       (Existing.Column = NormalizedEdit.Column) then
    begin
      if Existing.Snapshot <> NormalizedEdit.Snapshot then
        raise Exception.CreateFmt(
          'Conflicting inline snapshots at %s:%d:%d',
          [NormalizedEdit.SourcePath, NormalizedEdit.Line,
          NormalizedEdit.Column]);
      Exit;
    end;
  FInlineEdits.Add(NormalizedEdit);
end;

procedure TGocciaTestRunnerSnapshotHost.FlushInlineSnapshots;
var
  Edit, Existing: TGocciaInlineSnapshotEdit;
  Duplicate: Boolean;
begin
  if FInlineEdits.Count = 0 then
    Exit;
  if not HasPersistentSource then
    raise Exception.Create('Inline snapshots require a persistent source file');

  EnterCriticalSection(InlineSnapshotWriteLock);
  try
    if PendingInlineError <> '' then
    begin
      FInlineEdits.Clear;
      Exit;
    end;
    for Edit in FInlineEdits do
    begin
      Duplicate := False;
      for Existing in PendingInlineEdits do
        if (Existing.SourcePath = Edit.SourcePath) and
           (Existing.Line = Edit.Line) and
           (Existing.Column = Edit.Column) then
        begin
          if Existing.Snapshot <> Edit.Snapshot then
          begin
            PendingInlineError := Format(
              'Conflicting inline snapshots at %s:%d:%d',
              [Edit.SourcePath, Edit.Line, Edit.Column]);
            PendingInlineEdits.Clear;
            Exit;
          end;
          Duplicate := True;
          Break;
        end;
      if not Duplicate then
        PendingInlineEdits.Add(Edit);
    end;
    FInlineEdits.Clear;
  finally
    LeaveCriticalSection(InlineSnapshotWriteLock);
  end;
end;

procedure ResetPendingInlineSnapshots;
begin
  EnterCriticalSection(InlineSnapshotWriteLock);
  try
    PendingInlineEdits.Clear;
    PendingInlineError := '';
  finally
    LeaveCriticalSection(InlineSnapshotWriteLock);
  end;
end;

procedure FlushPendingInlineSnapshots;
var
  Edit: TGocciaInlineSnapshotEdit;
  CurrentPath, Source: string;
begin
  EnterCriticalSection(InlineSnapshotWriteLock);
  try
    if PendingInlineError <> '' then
      raise Exception.Create(PendingInlineError);
    if PendingInlineEdits.Count = 0 then
      Exit;
    PendingInlineEdits.Sort(
      TComparer<TGocciaInlineSnapshotEdit>.Construct(CompareInlineEdits));
    CurrentPath := '';
    for Edit in PendingInlineEdits do
    begin
      if Edit.SourcePath <> CurrentPath then
      begin
        if CurrentPath <> '' then
          WriteUTF8TextFile(CurrentPath, UTF8String(Source));
        CurrentPath := Edit.SourcePath;
        if not FileExists(CurrentPath) then
          raise Exception.Create('Inline snapshot source does not exist: ' +
            CurrentPath);
        Source := string(ReadUTF8FileText(CurrentPath));
      end;
      Source := ApplyInlineEdit(Source, Edit);
    end;
    if CurrentPath <> '' then
      WriteUTF8TextFile(CurrentPath, UTF8String(Source));
  finally
    PendingInlineEdits.Clear;
    PendingInlineError := '';
    LeaveCriticalSection(InlineSnapshotWriteLock);
  end;
end;

initialization
  InitCriticalSection(InlineSnapshotWriteLock);
  PendingInlineEdits := TList<TGocciaInlineSnapshotEdit>.Create;

finalization
  PendingInlineEdits.Free;
  DoneCriticalSection(InlineSnapshotWriteLock);

end.
