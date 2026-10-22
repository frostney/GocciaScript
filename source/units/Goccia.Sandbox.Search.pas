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

{ Lines split at LF; a CR before the LF and a final LF are not content. }
procedure SplitLines(const AText: string; const AFirst: Integer;
  out ALines: TSandboxGrepLines);
var
  Index, Start, LineLength: Integer;

  procedure Add(const AStart, ALength: Integer);
  begin
    if ALines.Count >= Length(ALines.Starts) then
    begin
      SetLength(ALines.Starts, ALines.Count * 2 + 16);
      SetLength(ALines.Lengths, ALines.Count * 2 + 16);
    end;
    ALines.Starts[ALines.Count] := AStart;
    ALines.Lengths[ALines.Count] := ALength;
    Inc(ALines.Count);
  end;

begin
  ALines := Default(TSandboxGrepLines);
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

function SandboxGrep(const AFileSystem: TSandboxVirtualFileSystem;
  const AFiles: TSandboxSearchFileArray;
  const AOptions: TGocciaSandboxGrepOptions): TGocciaSandboxGrepResult;
var
  Source, Flags, Text, Line: string;
  CompiledProgram: TRegExpProgram;
  MatchResult: TGocciaRegExpMatchResult;
  Literal: Boolean;
  FileIndex, LineIndex, ContextIndex, Column, Count, ErrorOffset,
    First, ContextStart, ContextEnd: Integer;
  BytesRead, TextBytes, LineBytes, KeptBytes: Int64;
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

  Count := 0;
  BytesRead := 0;
  try
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

      { The raw bytes, then their UTF-16 decoding, are held while the file
        is searched. }
      TextBytes := AFiles[FileIndex].Size * (1 + BYTES_PER_CODE_UNIT);
      ReserveNativeBytes(TextBytes);
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
        SplitLines(Text, First, Lines);
        for LineIndex := 0 to Lines.Count - 1 do
        begin
          if (LineIndex and $FF) = 0 then
            CheckNativeWork;
          Line := LineAt(LineIndex);
          if Literal then
            Column := Pos(AOptions.Pattern, Line)
          else
          begin
            LineBytes := Int64(Length(Line)) * REGEXP_INPUT_BYTES_PER_CODE_UNIT;
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
          Match.Text := Line;
          KeptBytes := Int64(Length(Line)) * BYTES_PER_CODE_UNIT;
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
        ReleaseNativeBytes(TextBytes);
      end;
      if Result.Truncated then
        Break;
    end;
  except
    ReleaseNativeBytes(Result.ReservedBytes);
    raise;
  end;
  SetLength(Result.Matches, Count);
end;

end.
