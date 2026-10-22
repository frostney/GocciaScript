unit Goccia.Sandbox.Search;

{ Content search over the sandbox virtual filesystem, behind goccia:search's
  grep: each selected file is decoded as UTF-8 and every line is matched with
  the engine's own RegExp. docs/built-ins-sandbox-search.md is the
  specification and fixtures/sandbox-search/vectors.json pins it.

  The work is bounded the way the rest of the engine's native work is: each
  line and file polls the run's timeout and instruction limit, the RegExp VM
  keeps its step limit, and the decoded text and every kept result are
  reserved against the run's memory limit until the caller releases them. }

{$I Goccia.inc}

interface

uses
  SysUtils,

  SandboxGlob,
  SandboxVirtualFileSystem;

const
  DEFAULT_SANDBOX_GREP_MAX_RESULTS = 1000;
  { No byte limit: the files' own sizes bound the read. }
  SANDBOX_GREP_NO_BYTE_LIMIT = -1;
  { More context than any file has lines; larger requests are clamped. }
  MAX_SANDBOX_GREP_CONTEXT = MaxInt div 4;

type
  TGocciaSandboxGrepOptions = record
    Pattern: string;
    Regex: Boolean;
    IgnoreCase: Boolean;
    MaxResults: Integer;
    MaxBytes: Int64;
    Context: Integer;
  end;

  TGocciaSandboxGrepMatch = record
    Path: string;
    Line: Integer;
    Column: Integer;
    Text: string;
    Before: TStringArray;
    After: TStringArray;
  end;

  TGocciaSandboxGrepMatchArray = array of TGocciaSandboxGrepMatch;

  TGocciaSandboxGrepResult = record
    Matches: TGocciaSandboxGrepMatchArray;
    Truncated: Boolean;
    { Native bytes reserved for the matches' text; the caller releases them
      with ReleaseNativeBytes once the matches are script values. }
    ReservedBytes: Int64;
  end;

{ Compiles the pattern as the RegExp constructor would; an invalid pattern
  throws the script's SyntaxError. Then searches AFiles, which
  SandboxSearchFiles selected and sorted. }
function SandboxGrep(const AFileSystem: TSandboxVirtualFileSystem;
  const AFiles: TSandboxSearchFileArray;
  const AOptions: TGocciaSandboxGrepOptions): TGocciaSandboxGrepResult;

implementation

uses
  TextEncoding,

  Goccia.EngineFault,
  Goccia.NativeLimits,
  Goccia.RegExp.&Program,
  Goccia.RegExp.Engine,
  Goccia.RegExp.Runtime,
  Goccia.RegExp.VM,
  Goccia.Values.ErrorHelper;

const
  REGEXP_SYNTAX_CHARACTERS = '\^$.*+?()[]{}|/';
  IGNORE_CASE_FLAGS = 'i';
  NO_FLAGS = '';
  BYTE_ORDER_MARK = #$FEFF;
  LINE_FEED = #10;
  CARRIAGE_RETURN = #13;
  { A decoded UTF-16 string costs two bytes a code unit; the RegExp VM's
    decoded subject costs four more. }
  BYTES_PER_CODE_UNIT = SizeOf(Char);
  REGEXP_INPUT_BYTES_PER_CODE_UNIT = SizeOf(Cardinal);
  { A file's raw byte, up to two decoder buffer bytes and two bytes of the
    decoded copy. }
  DECODE_BYTES_PER_BYTE = 1 + 2 * BYTES_PER_CODE_UNIT;
  { A line's start and length. }
  LINE_INDEX_BYTES = 2 * SizeOf(Integer);

function EscapeLiteral(const AText: string): string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 1 to Length(AText) do
  begin
    if Pos(AText[Index], REGEXP_SYNTAX_CHARACTERS) > 0 then
      Result := Result + '\';
    Result := Result + AText[Index];
  end;
end;

function ContainsNul(const ABytes: TBytes): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(ABytes) do
    if ABytes[Index] = 0 then
      Exit(True);
  Result := False;
end;

type
  TSandboxGrepLines = record
    Starts: array of Integer;
    Lengths: array of Integer;
    Count: Integer;
  end;

  TSandboxGrepFailureTable = array of Integer;

{ The number of lines SplitLines finds, so its arrays are sized (and their
  memory reserved) exactly. }
function CountLines(const AText: string; const AFirst: Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := AFirst to Length(AText) do
    if AText[Index] = LINE_FEED then
      Inc(Result);
  if (Length(AText) >= AFirst) and (AText[Length(AText)] <> LINE_FEED) then
    Inc(Result);
end;

{ Lines split at LF; a CR before the LF, a CR ending the last line and a
  final LF are not content. }
procedure SplitLines(const AText: string; const AFirst, ACount: Integer;
  out ALines: TSandboxGrepLines);
var
  Index, Start, LineLength: Integer;

  procedure Add(const AStart, ALength: Integer);
  begin
    ALines.Starts[ALines.Count] := AStart;
    ALines.Lengths[ALines.Count] := ALength;
    Inc(ALines.Count);
  end;

begin
  ALines := Default(TSandboxGrepLines);
  SetLength(ALines.Starts, ACount);
  SetLength(ALines.Lengths, ACount);
  Start := AFirst;
  for Index := AFirst to Length(AText) do
    if AText[Index] = LINE_FEED then
    begin
      LineLength := Index - Start;
      if (LineLength > 0) and (AText[Index - 1] = CARRIAGE_RETURN) then
        Dec(LineLength);
      Add(Start, LineLength);
      Start := Index + 1;
    end;
  if Start <= Length(AText) then
  begin
    LineLength := Length(AText) - Start + 1;
    if AText[Length(AText)] = CARRIAGE_RETURN then
      Dec(LineLength);
    Add(Start, LineLength);
  end;
end;

{ Knuth-Morris-Pratt failure links for a literal search that is linear in
  the line, whatever the pattern. }
function BuildFailureTable(const APattern: string): TSandboxGrepFailureTable;
var
  Index, Matched: Integer;
begin
  Result := nil;
  SetLength(Result, Length(APattern) + 1);
  Matched := 0;
  for Index := 2 to Length(APattern) do
  begin
    while (Matched > 0) and (APattern[Index] <> APattern[Matched + 1]) do
      Matched := Result[Matched];
    if APattern[Index] = APattern[Matched + 1] then
      Inc(Matched);
    Result[Index] := Matched;
  end;
end;

{ The 1-based column of the first occurrence of APattern in ALength code
  units of AText from AStart, or 0. Polls the run's limits as it goes. }
function FindLiteral(const AText: string; const AStart, ALength: Integer;
  const APattern: string; const AFailure: TSandboxGrepFailureTable): Integer;
const
  POLL_MASK = $FFFF;
var
  Index, Matched: Integer;
begin
  Matched := 0;
  for Index := 0 to ALength - 1 do
  begin
    if (Index and POLL_MASK) = POLL_MASK then
      CheckNativeWork;
    while (Matched > 0) and
          (AText[AStart + Index] <> APattern[Matched + 1]) do
      Matched := AFailure[Matched];
    if AText[AStart + Index] = APattern[Matched + 1] then
      Inc(Matched);
    if Matched = Length(APattern) then
      Exit(Index - Length(APattern) + 2);
  end;
  Result := 0;
end;

function SandboxGrep(const AFileSystem: TSandboxVirtualFileSystem;
  const AFiles: TSandboxSearchFileArray;
  const AOptions: TGocciaSandboxGrepOptions): TGocciaSandboxGrepResult;
var
  Source, Flags, Text, Line: string;
  CompiledProgram: TRegExpProgram;
  MatchResult: TGocciaRegExpMatchResult;
  Literal: Boolean;
  Failure: TSandboxGrepFailureTable;
  FileIndex, LineIndex, ContextIndex, Column, Count, ErrorOffset,
    First, ContextStart, ContextEnd, LineCount: Integer;
  BytesRead, TextBytes, LineBytes, KeptBytes, FailureBytes, IndexBytes: Int64;
  Bytes: TBytes;
  Lines: TSandboxGrepLines;
  Match: TGocciaSandboxGrepMatch;

  function LineAt(const AIndex: Integer): string;
  begin
    Result := Copy(Text, Lines.Starts[AIndex], Lines.Lengths[AIndex]);
  end;

begin
  Result := Default(TGocciaSandboxGrepResult);
  Literal := not AOptions.Regex and not AOptions.IgnoreCase;
  if AOptions.Regex then
    Source := AOptions.Pattern
  else
    Source := EscapeLiteral(AOptions.Pattern);
  if AOptions.IgnoreCase then
    Flags := IGNORE_CASE_FLAGS
  else
    Flags := NO_FLAGS;
  try
    CompiledProgram := CompileRegExpProgram(Source, Flags);
  except
    on E: Exception do
    begin
      if IsEngineIntegrityFault(E) then
        raise;
      ThrowSyntaxError(E.Message);
    end;
  end;

  FailureBytes := 0;
  if Literal then
  begin
    FailureBytes := CheckedNativeByteSize(Int64(Length(AOptions.Pattern)) + 1,
      SizeOf(Integer));
    ReserveNativeBytes(FailureBytes);
  end;
  Count := 0;
  BytesRead := 0;
  try
    if Literal then
      Failure := BuildFailureTable(AOptions.Pattern);
    for FileIndex := 0 to High(AFiles) do
    begin
      CheckNativeWork;
      if (AOptions.MaxBytes <> SANDBOX_GREP_NO_BYTE_LIMIT) and
         (BytesRead + AFiles[FileIndex].Size > AOptions.MaxBytes) then
      begin
        Result.Truncated := True;
        Break;
      end;
      Inc(BytesRead, AFiles[FileIndex].Size);

      { Held while the file is searched: its raw bytes, the decoder's buffer
        and the decoded text it copies out, then the line index. }
      TextBytes := CheckedNativeByteSize(AFiles[FileIndex].Size,
        DECODE_BYTES_PER_BYTE);
      ReserveNativeBytes(TextBytes);
      IndexBytes := 0;
      try
        Bytes := AFileSystem.SnapshotReadAllBytes(AFiles[FileIndex].Path);
        if ContainsNul(Bytes) or not TryDecodeUTF8(Bytes, Text,
          ErrorOffset) then
        begin
          Bytes := nil;
          Continue;
        end;
        Bytes := nil;
        First := 1;
        if (Length(Text) > 0) and (Text[1] = BYTE_ORDER_MARK) then
          First := 2;
        LineCount := CountLines(Text, First);
        IndexBytes := CheckedNativeByteSize(LineCount, LINE_INDEX_BYTES);
        ReserveNativeBytes(IndexBytes);
        SplitLines(Text, First, LineCount, Lines);
        for LineIndex := 0 to Lines.Count - 1 do
        begin
          CheckNativeWork;
          if Literal then
            Column := FindLiteral(Text, Lines.Starts[LineIndex],
              Lines.Lengths[LineIndex], AOptions.Pattern, Failure)
          else
          begin
            Line := LineAt(LineIndex);
            LineBytes := Int64(Length(Line)) *
              (REGEXP_INPUT_BYTES_PER_CODE_UNIT + BYTES_PER_CODE_UNIT);
            ReserveNativeBytes(LineBytes);
            try
              try
                if ExecuteCompiledRegExp(CompiledProgram, Source, Flags, Line,
                  0, False, MatchResult) then
                  Column := MatchResult.MatchIndex + 1
                else
                  Column := 0;
              except
                on E: ERegExpRuntimeError do
                  ThrowRegExpRuntimeError(E);
              end;
            finally
              ReleaseNativeBytes(LineBytes);
            end;
          end;
          if Column = 0 then
            Continue;
          if Count = AOptions.MaxResults then
          begin
            Result.Truncated := True;
            Break;
          end;
          Match := Default(TGocciaSandboxGrepMatch);
          Match.Path := AFiles[FileIndex].Path;
          Match.Line := LineIndex + 1;
          Match.Column := Column;
          Match.Text := LineAt(LineIndex);
          KeptBytes := Int64(Length(Match.Text)) * BYTES_PER_CODE_UNIT;
          if AOptions.Context > 0 then
          begin
            ContextStart := LineIndex - AOptions.Context;
            if ContextStart < 0 then
              ContextStart := 0;
            SetLength(Match.Before, LineIndex - ContextStart);
            for ContextIndex := ContextStart to LineIndex - 1 do
            begin
              Match.Before[ContextIndex - ContextStart] := LineAt(ContextIndex);
              Inc(KeptBytes, Int64(Lines.Lengths[ContextIndex]) *
                BYTES_PER_CODE_UNIT);
            end;
            ContextEnd := LineIndex + AOptions.Context;
            if ContextEnd > Lines.Count - 1 then
              ContextEnd := Lines.Count - 1;
            SetLength(Match.After, ContextEnd - LineIndex);
            for ContextIndex := LineIndex + 1 to ContextEnd do
            begin
              Match.After[ContextIndex - LineIndex - 1] := LineAt(ContextIndex);
              Inc(KeptBytes, Int64(Lines.Lengths[ContextIndex]) *
                BYTES_PER_CODE_UNIT);
            end;
          end;
          ReserveNativeBytes(KeptBytes);
          Inc(Result.ReservedBytes, KeptBytes);
          if Count >= Length(Result.Matches) then
            SetLength(Result.Matches, Count * 2 + 16);
          Result.Matches[Count] := Match;
          Inc(Count);
        end;
      finally
        Text := '';
        Lines := Default(TSandboxGrepLines);
        ReleaseNativeBytes(IndexBytes);
        ReleaseNativeBytes(TextBytes);
      end;
      if Result.Truncated then
        Break;
    end;
  except
    ReleaseNativeBytes(FailureBytes);
    ReleaseNativeBytes(Result.ReservedBytes);
    raise;
  end;
  Failure := nil;
  ReleaseNativeBytes(FailureBytes);
  SetLength(Result.Matches, Count);
end;

end.
