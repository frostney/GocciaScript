program ConsoleOutput.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  ConsoleOutput,
  ProcessEncoding,
  TestingPascalLibrary;

const
  // A code page in which most of the test text has no characters, as a
  // Windows console's standard output starts in.
  CONSOLE_CODE_PAGE = 850;
  LINE: UnicodeString = #$00FC' '#$D83D#$DE00' '#$65E5#$672C#$8A9E;

type
  TConsoleOutputTests = class(TTestSuite)
  private
    FPath: string;
    FTarget: Text;
    procedure OpenTarget;
    procedure CloseTarget;
    function TargetFileBytes: RawByteString;
    procedure TestCompleteUTF8LengthKeepsWholeCharacters;
    procedure TestCompleteUTF8LengthHoldsBackASplitCharacter;
    procedure TestCompleteUTF8LengthIgnoresMalformedTails;
    procedure TestRedirectedOutputWritesUTF8Bytes;
    procedure TestConsoleOutputGoesToTheWriter;
    procedure TestConsoleOutputJoinsCharactersSplitAcrossFlushes;
    procedure TestConsoleOutputFlushesEachLine;
    procedure TestConsoleOutputWritesAnIncompleteTailOnClose;
    procedure TestConsoleWriterFailureSetsIOResult;
  protected
    procedure BeforeEach; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

var
  Probed: Boolean;
  ProbedHandle: THandle;
  WriterCalls: Integer;
  WriterHandle: THandle;
  Written: RawByteString;
  WrittenCharacters: UnicodeString;
  WriterSucceeds: Boolean;

function ProbeConsole(const AHandle: THandle): Boolean;
begin
  Probed := True;
  ProbedHandle := AHandle;
  Result := True;
end;

function ProbeRedirected(const AHandle: THandle): Boolean;
begin
  Probed := True;
  ProbedHandle := AHandle;
  Result := False;
end;

function RecordWrite(const AHandle: THandle; const AText: PAnsiChar;
  const ALength: SizeInt): Boolean;
var
  Chunk: RawByteString;
begin
  Inc(WriterCalls);
  WriterHandle := AHandle;
  SetString(Chunk, AText, ALength);
  Written := Written + Chunk;
  // Each call is decoded on its own, as the console writer converts it, so a
  // character split between two calls comes out as U+FFFD.
  WrittenCharacters := WrittenCharacters + UTF8Decode(Chunk);
  Result := WriterSucceeds;
end;

function UTF8Bytes(const AText: UnicodeString): RawByteString;
begin
  Result := UTF8Encode(AText);
  SetCodePage(Result, CP_NONE, False);
end;

function RawBytes(const ABytes: array of Byte): RawByteString;
var
  I: Integer;
begin
  SetLength(Result, Length(ABytes));
  for I := 0 to High(ABytes) do
    Result[I + 1] := AnsiChar(ABytes[I]);
end;

function CompleteLength(const ABytes: RawByteString): SizeInt;
begin
  Result := CompleteUTF8Length(PAnsiChar(ABytes), Length(ABytes));
end;

procedure TConsoleOutputTests.SetupTests;
begin
  Test('CompleteUTF8Length keeps text that ends on a character boundary',
    TestCompleteUTF8LengthKeepsWholeCharacters);
  Test('CompleteUTF8Length holds back a character split at the end',
    TestCompleteUTF8LengthHoldsBackASplitCharacter);
  Test('CompleteUTF8Length holds back nothing more bytes cannot complete',
    TestCompleteUTF8LengthIgnoresMalformedTails);
  Test('A redirected handle gets UTF-8 bytes and keeps its write function',
    TestRedirectedOutputWritesUTF8Bytes);
  Test('A console handle gets UTF-8 through the writer',
    TestConsoleOutputGoesToTheWriter);
  Test('A character split across two flushes reaches the writer whole',
    TestConsoleOutputJoinsCharactersSplitAcrossFlushes);
  Test('Each WriteLn to a console is written before it returns',
    TestConsoleOutputFlushesEachLine);
  Test('Closing a console file writes an incomplete character it holds',
    TestConsoleOutputWritesAnIncompleteTailOnClose);
  Test('A failed console write sets IOResult',
    TestConsoleWriterFailureSetsIOResult);
end;

procedure TConsoleOutputTests.BeforeEach;
begin
  // A failed assertion skips the rest of its test, so the file it left open
  // is closed here.
  CloseTarget;
  Probed := False;
  ProbedHandle := THandle(-1);
  WriterCalls := 0;
  WriterHandle := THandle(-1);
  Written := '';
  WrittenCharacters := '';
  WriterSucceeds := True;
end;

procedure TConsoleOutputTests.AfterAll;
begin
  CloseTarget;
end;

procedure TConsoleOutputTests.OpenTarget;
begin
  FPath := GetTempFileName(GetTempDir, 'goccia-console-output');
  AssignFile(FTarget, FPath);
  Rewrite(FTarget);
  SetTextCodePage(FTarget, CONSOLE_CODE_PAGE);
end;

procedure TConsoleOutputTests.CloseTarget;
begin
  if FPath = '' then
    Exit;
  if TextRec(FTarget).Mode <> fmClosed then
  begin
    {$I-}
    CloseFile(FTarget);
    {$I+}
    IOResult;
  end;
  DeleteFile(FPath);
  FPath := '';
end;

function TConsoleOutputTests.TargetFileBytes: RawByteString;
var
  Stream: TFileStream;
begin
  if TextRec(FTarget).Mode <> fmClosed then
    CloseFile(FTarget);
  Stream := TFileStream.Create(FPath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TConsoleOutputTests.TestCompleteUTF8LengthKeepsWholeCharacters;
begin
  Expect<Integer>(CompleteLength('')).ToBe(0);
  Expect<Integer>(CompleteLength('abc')).ToBe(3);
  Expect<Integer>(CompleteLength(UTF8Bytes('a' + #$00FC))).ToBe(3);
  Expect<Integer>(CompleteLength(UTF8Bytes(#$65E5#$672C))).ToBe(6);
  Expect<Integer>(CompleteLength(UTF8Bytes('x' + #$D83D#$DE00))).ToBe(5);
end;

procedure TConsoleOutputTests.TestCompleteUTF8LengthHoldsBackASplitCharacter;
begin
  // U+00FC is C3 BC, U+65E5 is E6 97 A5 and U+1F600 is F0 9F 98 80.
  Expect<Integer>(CompleteLength(RawBytes([$61, $C3]))).ToBe(1);
  Expect<Integer>(CompleteLength(RawBytes([$61, $E6]))).ToBe(1);
  Expect<Integer>(CompleteLength(RawBytes([$61, $E6, $97]))).ToBe(1);
  Expect<Integer>(CompleteLength(RawBytes([$61, $F0, $9F, $98]))).ToBe(1);
  Expect<Integer>(CompleteLength(RawBytes([$F0, $9F]))).ToBe(0);
end;

procedure TConsoleOutputTests.TestCompleteUTF8LengthIgnoresMalformedTails;
begin
  // A continuation byte with no lead before it, a run longer than any
  // sequence, and an invalid lead are written as they are: no further byte
  // can make them a character.
  Expect<Integer>(CompleteLength(RawBytes([$61, $80]))).ToBe(2);
  Expect<Integer>(CompleteLength(RawBytes([$80, $80, $80, $80]))).ToBe(4);
  Expect<Integer>(CompleteLength(RawBytes([$61, $F0, $9F, $98, $80, $80]))).ToBe(6);
  Expect<Integer>(CompleteLength(RawBytes([$61, $FF]))).ToBe(2);
  Expect<Integer>(CompleteLength(RawBytes([$C3, $61]))).ToBe(2);
end;

procedure TConsoleOutputTests.TestRedirectedOutputWritesUTF8Bytes;
var
  InOut: CodePointer;
begin
  OpenTarget;
  InOut := TextRec(FTarget).InOutFunc;
  UseUnicodeOutput(FTarget, ProbeRedirected, RecordWrite);
  Expect<Boolean>(Probed).ToBe(True);
  Expect<Boolean>(ProbedHandle = TextRec(FTarget).Handle).ToBe(True);
  Expect<Boolean>(TextRec(FTarget).InOutFunc = InOut).ToBe(True);
  WriteLn(FTarget, LINE);
  Expect<Integer>(WriterCalls).ToBe(0);
  Expect<Boolean>(TargetFileBytes = UTF8Bytes(LINE + LineEnding)).ToBe(True);
end;

procedure TConsoleOutputTests.TestConsoleOutputGoesToTheWriter;
begin
  OpenTarget;
  UseUnicodeOutput(FTarget, ProbeConsole, RecordWrite);
  Expect<Boolean>(Probed).ToBe(True);
  WriteLn(FTarget, LINE);
  Write(FTarget, 'plain ', 42);
  CloseFile(FTarget);
  Expect<Boolean>(WriterHandle = ProbedHandle).ToBe(True);
  Expect<string>(WrittenCharacters).ToBe(LINE + LineEnding + 'plain 42');
  Expect<Integer>(Length(TargetFileBytes)).ToBe(0);
end;

procedure TConsoleOutputTests.TestConsoleOutputJoinsCharactersSplitAcrossFlushes;
var
  Expected: string;
  I: Integer;
begin
  // Each emoji is four bytes in UTF-8, so a buffer of whole emoji after one
  // byte of ASCII ends in the middle of one, whatever the buffer size.
  Expected := 'a';
  for I := 1 to 1000 do
    Expected := Expected + #$D83D#$DE00;
  OpenTarget;
  UseUnicodeOutput(FTarget, ProbeConsole, RecordWrite);
  WriteLn(FTarget, Expected);
  CloseFile(FTarget);
  Expect<Boolean>(WriterCalls > 1).ToBe(True);
  Expect<string>(WrittenCharacters).ToBe(Expected + LineEnding);
end;

procedure TConsoleOutputTests.TestConsoleOutputFlushesEachLine;
begin
  OpenTarget;
  UseUnicodeOutput(FTarget, ProbeConsole, RecordWrite);
  WriteLn(FTarget, 'first');
  Expect<string>(WrittenCharacters).ToBe('first' + LineEnding);
  WriteLn(FTarget, LINE);
  Expect<string>(WrittenCharacters).ToBe('first' + LineEnding + LINE + LineEnding);
end;

procedure TConsoleOutputTests.TestConsoleOutputWritesAnIncompleteTailOnClose;
var
  Lead: ShortString;
begin
  // The lead byte of U+00FC with nothing after it, written as a raw byte.
  Lead := 'x';
  Lead[1] := AnsiChar($C3);
  OpenTarget;
  UseUnicodeOutput(FTarget, ProbeConsole, RecordWrite);
  Write(FTarget, Lead);
  CloseFile(FTarget);
  Expect<Boolean>(Written = RawBytes([$C3])).ToBe(True);
end;

procedure TConsoleOutputTests.TestConsoleWriterFailureSetsIOResult;
var
  Code: Word;
begin
  OpenTarget;
  UseUnicodeOutput(FTarget, ProbeConsole, RecordWrite);
  WriterSucceeds := False;
  {$I-}
  WriteLn(FTarget, 'lost');
  Code := IOResult;
  {$I+}
  Expect<Integer>(Code).ToBe(WRITE_FAILED_IO_RESULT);
end;

begin
  TestRunnerProgram.AddSuite(TConsoleOutputTests.Create('ConsoleOutput'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
