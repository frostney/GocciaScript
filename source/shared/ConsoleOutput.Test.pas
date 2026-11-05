program ConsoleOutput.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  ConsoleOutput,
  ProcessEncoding,
  TestingPascalLibrary;

type
  TConsoleOutputTests = class(TTestSuite)
  private
    FPath: string;
    procedure OpenOutput(var AOutput: Text);
    function FileBytes: RawByteString;
    procedure TestCompleteUTF8LengthKeepsWholeCharacters;
    procedure TestCompleteUTF8LengthHoldsBackASplitCharacter;
    procedure TestCompleteUTF8LengthIgnoresMalformedTails;
    procedure TestRedirectedOutputWritesUTF8Bytes;
    procedure TestConsoleOutputWritesUTF16;
    procedure TestConsoleOutputJoinsCharactersSplitAcrossFlushes;
    procedure TestConsoleOutputFlushesEachLine;
    procedure TestConsoleWriterFailureSetsIOResult;
  public
    procedure SetupTests; override;
  end;

var
  Probed: Boolean;
  ProbedHandle: THandle;
  WriterCalls: Integer;
  WriterHandle: THandle;
  Written: UnicodeString;
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

function RecordUTF16(const AHandle: THandle; const AText: PUnicodeChar;
  const ALength: SizeInt): Boolean;
var
  Chunk: UnicodeString;
begin
  Inc(WriterCalls);
  WriterHandle := AHandle;
  SetString(Chunk, AText, ALength);
  Written := Written + Chunk;
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
  Test('A console handle gets UTF-16 through the writer',
    TestConsoleOutputWritesUTF16);
  Test('A character split across two flushes reaches the console whole',
    TestConsoleOutputJoinsCharactersSplitAcrossFlushes);
  Test('Each WriteLn to a console is written before it returns',
    TestConsoleOutputFlushesEachLine);
  Test('A failed console write sets IOResult',
    TestConsoleWriterFailureSetsIOResult);
end;

procedure ResetRecording;
begin
  Probed := False;
  ProbedHandle := THandle(-1);
  WriterCalls := 0;
  WriterHandle := THandle(-1);
  Written := '';
  WriterSucceeds := True;
end;

procedure TConsoleOutputTests.OpenOutput(var AOutput: Text);
begin
  ResetRecording;
  FPath := GetTempFileName(GetTempDir, 'goccia-console-output');
  AssignFile(AOutput, FPath);
  Rewrite(AOutput);
end;

function TConsoleOutputTests.FileBytes: RawByteString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FPath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
  DeleteFile(FPath);
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
  Target: Text;
  InOut: CodePointer;
begin
  OpenOutput(Target);
  InOut := TextRec(Target).InOutFunc;
  UseUnicodeOutput(Target, ProbeRedirected, RecordUTF16);
  Expect<Boolean>(Probed).ToBe(True);
  Expect<Boolean>(ProbedHandle = TextRec(Target).Handle).ToBe(True);
  Expect<Boolean>(TextRec(Target).InOutFunc = InOut).ToBe(True);
  Expect<Integer>(GetTextCodePage(Target)).ToBe(CP_UTF8);
  WriteLn(Target, UnicodeString(#$00FC' '#$D83D#$DE00' '#$65E5#$672C#$8A9E));
  CloseFile(Target);
  Expect<Integer>(WriterCalls).ToBe(0);
  Expect<Boolean>(FileBytes =
    UTF8Bytes(#$00FC' '#$D83D#$DE00' '#$65E5#$672C#$8A9E + LineEnding)).ToBe(True);
end;

procedure TConsoleOutputTests.TestConsoleOutputWritesUTF16;
const
  LINE: UnicodeString = #$00FC' '#$D83D#$DE00' '#$65E5#$672C#$8A9E;
var
  Target: Text;
begin
  OpenOutput(Target);
  UseUnicodeOutput(Target, ProbeConsole, RecordUTF16);
  Expect<Boolean>(Probed).ToBe(True);
  WriteLn(Target, LINE);
  Write(Target, 'plain ', 42);
  CloseFile(Target);
  Expect<Boolean>(WriterHandle = ProbedHandle).ToBe(True);
  Expect<Boolean>(Written = LINE + LineEnding + 'plain 42').ToBe(True);
  Expect<Integer>(Length(FileBytes)).ToBe(0);
end;

procedure TConsoleOutputTests.TestConsoleOutputJoinsCharactersSplitAcrossFlushes;
var
  Target: Text;
  Line: UnicodeString;
  I: Integer;
begin
  // Each emoji is four bytes in UTF-8, so a buffer of whole emoji after one
  // byte of ASCII ends in the middle of one, whatever the buffer size.
  Line := 'a';
  for I := 1 to 1000 do
    Line := Line + #$D83D#$DE00;
  OpenOutput(Target);
  UseUnicodeOutput(Target, ProbeConsole, RecordUTF16);
  WriteLn(Target, Line);
  CloseFile(Target);
  Expect<Boolean>(WriterCalls > 1).ToBe(True);
  Expect<Boolean>(Written = Line + LineEnding).ToBe(True);
  Expect<Boolean>(Pos(UnicodeString(#$FFFD), Written) = 0).ToBe(True);
end;

procedure TConsoleOutputTests.TestConsoleOutputFlushesEachLine;
var
  Target: Text;
begin
  OpenOutput(Target);
  UseUnicodeOutput(Target, ProbeConsole, RecordUTF16);
  WriteLn(Target, 'first');
  Expect<Boolean>(Written = 'first' + LineEnding).ToBe(True);
  WriteLn(Target, UnicodeString(#$00FC));
  Expect<Boolean>(Written = 'first' + LineEnding + #$00FC + LineEnding).ToBe(True);
  CloseFile(Target);
  FileBytes;
end;

procedure TConsoleOutputTests.TestConsoleWriterFailureSetsIOResult;
var
  Target: Text;
  Code: Word;
begin
  OpenOutput(Target);
  UseUnicodeOutput(Target, ProbeConsole, RecordUTF16);
  WriterSucceeds := False;
  {$I-}
  WriteLn(Target, 'lost');
  Code := IOResult;
  {$I+}
  CloseFile(Target);
  FileBytes;
  Expect<Integer>(Code).ToBe(101);
end;

begin
  TestRunnerProgram.AddSuite(TConsoleOutputTests.Create('ConsoleOutput'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
