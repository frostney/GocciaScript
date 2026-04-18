program StringBuffer.Test;

{$I Shared.inc}

uses
  SysUtils,

  StringBuffer,
  TestingPascalLibrary;

type
  TStringBufferTests = class(TTestSuite)
  private
    procedure TestEmptyBufferLengthIsZero;
    procedure TestEmptyBufferToStringReturnsEmpty;
    procedure TestAppendString;
    procedure TestAppendChar;
    procedure TestMultipleAppendsConcatenate;
    procedure TestAppendEmptyStringIsNoop;
    procedure TestClearResetsLength;
    procedure TestAppendAfterClearGivesFreshContent;
    procedure TestGrowBeyondSmallCapacity;
    procedure TestLargeAppendForcesMultipleDoublings;
    procedure TestMixedAppendCharAndAppend;
    procedure TestAppendCharSpecialCharacters;
    procedure TestRepeatedClearAndReuseCycles;
    procedure TestToStringDoesNotConsumeBuffer;
    procedure TestCreateWithExplicitCapacity;
    procedure TestCreateWithDefaultCapacity;
    procedure TestCreateWithZeroCapacityUsesDefault;
  public
    procedure SetupTests; override;
  end;

procedure TStringBufferTests.SetupTests;
begin
  Test('Empty buffer has length zero', TestEmptyBufferLengthIsZero);
  Test('Empty buffer ToString returns empty string', TestEmptyBufferToStringReturnsEmpty);
  Test('Append string increases length', TestAppendString);
  Test('AppendChar adds single character', TestAppendChar);
  Test('Multiple appends concatenate correctly', TestMultipleAppendsConcatenate);
  Test('Appending empty string is a no-op', TestAppendEmptyStringIsNoop);
  Test('Clear resets length to zero', TestClearResetsLength);
  Test('Append after Clear gives fresh content with no ghost data', TestAppendAfterClearGivesFreshContent);
  Test('Growth from small capacity preserves content integrity', TestGrowBeyondSmallCapacity);
  Test('Large append forces multiple capacity doublings', TestLargeAppendForcesMultipleDoublings);
  Test('Mixed AppendChar and Append produce correct result', TestMixedAppendCharAndAppend);
  Test('AppendChar handles special characters', TestAppendCharSpecialCharacters);
  Test('Repeated Clear and reuse cycles work correctly', TestRepeatedClearAndReuseCycles);
  Test('ToString does not consume the buffer', TestToStringDoesNotConsumeBuffer);
  Test('Create with explicit capacity works', TestCreateWithExplicitCapacity);
  Test('Create with default capacity works', TestCreateWithDefaultCapacity);
  Test('Create with zero capacity falls back to default', TestCreateWithZeroCapacityUsesDefault);
end;

procedure TStringBufferTests.TestEmptyBufferLengthIsZero;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Expect<Integer>(Buf.Length).ToBe(0);
end;

procedure TStringBufferTests.TestEmptyBufferToStringReturnsEmpty;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Expect<string>(Buf.ToString).ToBe('');
end;

procedure TStringBufferTests.TestAppendString;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.Append('hello');
  Expect<Integer>(Buf.Length).ToBe(5);
  Expect<string>(Buf.ToString).ToBe('hello');
end;

procedure TStringBufferTests.TestAppendChar;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.AppendChar('x');
  Expect<Integer>(Buf.Length).ToBe(1);
  Expect<string>(Buf.ToString).ToBe('x');
end;

procedure TStringBufferTests.TestMultipleAppendsConcatenate;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.Append('hello');
  Buf.Append(' ');
  Buf.Append('world');
  Expect<Integer>(Buf.Length).ToBe(11);
  Expect<string>(Buf.ToString).ToBe('hello world');
end;

procedure TStringBufferTests.TestAppendEmptyStringIsNoop;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.Append('data');
  Buf.Append('');
  Buf.Append('');
  Expect<Integer>(Buf.Length).ToBe(4);
  Expect<string>(Buf.ToString).ToBe('data');
end;

procedure TStringBufferTests.TestClearResetsLength;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.Append('some data here');
  Buf.Clear;
  Expect<Integer>(Buf.Length).ToBe(0);
  Expect<string>(Buf.ToString).ToBe('');
end;

procedure TStringBufferTests.TestAppendAfterClearGivesFreshContent;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.Append('old content that should vanish');
  Buf.Clear;
  Buf.Append('new');
  Expect<Integer>(Buf.Length).ToBe(3);
  Expect<string>(Buf.ToString).ToBe('new');
end;

procedure TStringBufferTests.TestGrowBeyondSmallCapacity;
var
  Buf: TStringBuffer;
  I: Integer;
  Expected: string;
begin
  Buf := TStringBuffer.Create(4);
  Expected := '';
  for I := 1 to 50 do
  begin
    Buf.AppendChar(AnsiChar(Ord('A') + ((I - 1) mod 26)));
    Expected := Expected + Char(Ord('A') + ((I - 1) mod 26));
  end;
  Expect<Integer>(Buf.Length).ToBe(50);
  Expect<string>(Buf.ToString).ToBe(Expected);
end;

procedure TStringBufferTests.TestLargeAppendForcesMultipleDoublings;
var
  Buf: TStringBuffer;
  LargeStr: AnsiString;
  I: Integer;
begin
  Buf := TStringBuffer.Create(4);
  // Build a 1000-char string: capacity 4 -> 8 -> 16 -> 32 -> 64 -> 128 -> 256 -> 512 -> 1024
  SetLength(LargeStr, 1000);
  for I := 1 to 1000 do
    LargeStr[I] := AnsiChar(Ord('a') + (I mod 26));
  Buf.Append(LargeStr);
  Expect<Integer>(Buf.Length).ToBe(1000);
  Expect<string>(Buf.ToString).ToBe(string(LargeStr));
end;

procedure TStringBufferTests.TestMixedAppendCharAndAppend;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.AppendChar('[');
  Buf.Append('item1');
  Buf.AppendChar(',');
  Buf.Append('item2');
  Buf.AppendChar(']');
  Expect<string>(Buf.ToString).ToBe('[item1,item2]');
  Expect<Integer>(Buf.Length).ToBe(13);
end;

procedure TStringBufferTests.TestAppendCharSpecialCharacters;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Buf.AppendChar(#0);    // null byte
  Buf.AppendChar(#10);   // newline
  Buf.AppendChar(#9);    // tab
  Buf.AppendChar(#255);  // high-byte char
  Expect<Integer>(Buf.Length).ToBe(4);
  Expect<Boolean>(Buf.ToString[1] = #0).ToBe(True);
  Expect<Boolean>(Buf.ToString[2] = #10).ToBe(True);
  Expect<Boolean>(Buf.ToString[3] = #9).ToBe(True);
  Expect<Boolean>(Buf.ToString[4] = #255).ToBe(True);
end;

procedure TStringBufferTests.TestRepeatedClearAndReuseCycles;
var
  Buf: TStringBuffer;
  Cycle: Integer;
begin
  Buf := TStringBuffer.Create(8);
  for Cycle := 1 to 5 do
  begin
    Buf.Clear;
    Buf.Append('cycle');
    Buf.Append(IntToStr(Cycle));
  end;
  Expect<string>(Buf.ToString).ToBe('cycle5');
  Expect<Integer>(Buf.Length).ToBe(6);
end;

procedure TStringBufferTests.TestToStringDoesNotConsumeBuffer;
var
  Buf: TStringBuffer;
  First, Second: string;
begin
  Buf := TStringBuffer.Create;
  Buf.Append('persistent');
  First := Buf.ToString;
  Second := Buf.ToString;
  Expect<string>(First).ToBe('persistent');
  Expect<string>(Second).ToBe('persistent');
  Expect<Integer>(Buf.Length).ToBe(10);
end;

procedure TStringBufferTests.TestCreateWithExplicitCapacity;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create(128);
  Expect<Integer>(Buf.Length).ToBe(0);
  Buf.Append('test');
  Expect<string>(Buf.ToString).ToBe('test');
end;

procedure TStringBufferTests.TestCreateWithDefaultCapacity;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create;
  Expect<Integer>(Buf.Length).ToBe(0);
  // Appending up to default capacity (64) should not cause issues
  Buf.Append('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789__');
  Expect<Integer>(Buf.Length).ToBe(64);
  Expect<string>(Buf.ToString).ToBe('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789__');
end;

procedure TStringBufferTests.TestCreateWithZeroCapacityUsesDefault;
var
  Buf: TStringBuffer;
begin
  Buf := TStringBuffer.Create(0);
  // Should fall back to DEFAULT_CAPACITY (64) and work fine
  Expect<Integer>(Buf.Length).ToBe(0);
  Buf.Append('works');
  Expect<string>(Buf.ToString).ToBe('works');
end;

begin
  TestRunnerProgram.AddSuite(TStringBufferTests.Create('StringBuffer'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
