program CLI.Units.Test;

{$I Shared.inc}

uses
  SysUtils,

  CLI.Units,
  TestingPascalLibrary;

type
  TCLIUnitsTests = class(TTestSuite)
  private
    procedure ExpectBytes(const AText: string; const AExpected: Int64);
    procedure ExpectBytesRejected(const AText, AExpectedError: string);
    procedure ExpectDuration(const AText: string; const AExpected: Int64);
    procedure ExpectDurationRejected(const AText, AExpectedError: string);
    procedure TestByteSizesAccepted;
    procedure TestByteSizesRejected;
    procedure TestAmbiguousByteSuffixes;
    procedure TestByteSizeOverflow;
    procedure TestDurationsAccepted;
    procedure TestDurationsRejected;
    procedure TestCounts;
  public
    procedure SetupTests; override;
  end;

procedure TCLIUnitsTests.SetupTests;
begin
  Test('Byte sizes accept plain bytes and binary suffixes',
    TestByteSizesAccepted);
  Test('Byte sizes reject signs, fractions, and whitespace',
    TestByteSizesRejected);
  Test('Decimal-looking byte suffixes are ambiguous',
    TestAmbiguousByteSuffixes);
  Test('Byte sizes reject overflow', TestByteSizeOverflow);
  Test('Durations accept plain milliseconds, ms, s, and m',
    TestDurationsAccepted);
  Test('Durations reject other suffixes', TestDurationsRejected);
  Test('Counts accept non-negative whole numbers only', TestCounts);
end;

procedure TCLIUnitsTests.ExpectBytes(const AText: string;
  const AExpected: Int64);
var
  Bytes: Int64;
  ErrorText: string;
begin
  Expect<Boolean>(TryParseByteSize(AText, Bytes, ErrorText)).ToBe(True);
  Expect<Int64>(Bytes).ToBe(AExpected);
  Expect<string>(ErrorText).ToBe('');
end;

procedure TCLIUnitsTests.ExpectBytesRejected(const AText,
  AExpectedError: string);
var
  Bytes: Int64;
  ErrorText: string;
begin
  Expect<Boolean>(TryParseByteSize(AText, Bytes, ErrorText)).ToBe(False);
  Expect<string>(ErrorText).ToBe(AExpectedError);
end;

procedure TCLIUnitsTests.ExpectDuration(const AText: string;
  const AExpected: Int64);
var
  Milliseconds: Int64;
  ErrorText: string;
begin
  Expect<Boolean>(TryParseDurationMilliseconds(AText, Milliseconds,
    ErrorText)).ToBe(True);
  Expect<Int64>(Milliseconds).ToBe(AExpected);
end;

procedure TCLIUnitsTests.ExpectDurationRejected(const AText,
  AExpectedError: string);
var
  Milliseconds: Int64;
  ErrorText: string;
begin
  Expect<Boolean>(TryParseDurationMilliseconds(AText, Milliseconds,
    ErrorText)).ToBe(False);
  Expect<string>(ErrorText).ToBe(AExpectedError);
end;

procedure TCLIUnitsTests.TestByteSizesAccepted;
begin
  ExpectBytes('0', 0);
  ExpectBytes('1024', 1024);
  ExpectBytes('1024B', 1024);
  ExpectBytes('64KiB', 64 * 1024);
  ExpectBytes('64kib', 64 * 1024);
  ExpectBytes('64Ki', 64 * 1024);
  ExpectBytes('8MiB', 8 * 1024 * 1024);
  ExpectBytes('2GiB', Int64(2) * 1024 * 1024 * 1024);
end;

procedure TCLIUnitsTests.TestByteSizesRejected;
begin
  ExpectBytesRejected('1.5MiB', BYTE_SIZE_ERROR);
  ExpectBytesRejected('-1', BYTE_SIZE_ERROR);
  ExpectBytesRejected('+1', BYTE_SIZE_ERROR);
  ExpectBytesRejected(' 5MiB', BYTE_SIZE_ERROR);
  ExpectBytesRejected('5MiB ', BYTE_SIZE_ERROR);
  ExpectBytesRejected('', BYTE_SIZE_ERROR);
  ExpectBytesRejected('MiB', BYTE_SIZE_ERROR);
  ExpectBytesRejected('5TiB', BYTE_SIZE_ERROR);
end;

procedure TCLIUnitsTests.TestAmbiguousByteSuffixes;
begin
  ExpectBytesRejected('64MB',
    '"MB" is ambiguous; use KiB, MiB, or GiB, or a plain byte count');
  ExpectBytesRejected('64k',
    '"k" is ambiguous; use KiB, MiB, or GiB, or a plain byte count');
  ExpectBytesRejected('1G',
    '"G" is ambiguous; use KiB, MiB, or GiB, or a plain byte count');
end;

procedure TCLIUnitsTests.TestByteSizeOverflow;
begin
  ExpectBytesRejected('99999999999999999999', TOO_LARGE_ERROR);
  ExpectBytesRejected('9223372036854775807GiB', TOO_LARGE_ERROR);
  ExpectBytes('9223372036854775807', High(Int64));
end;

procedure TCLIUnitsTests.TestDurationsAccepted;
begin
  ExpectDuration('0', 0);
  ExpectDuration('500', 500);
  ExpectDuration('500ms', 500);
  ExpectDuration('5s', 5000);
  ExpectDuration('2m', 120000);
  { Units are case-insensitive, like byte-size units. }
  ExpectDuration('5S', 5000);
  ExpectDuration('500MS', 500);
  ExpectDuration('2M', 120000);
end;

procedure TCLIUnitsTests.TestDurationsRejected;
begin
  ExpectDurationRejected('5h', DURATION_ERROR);
  ExpectDurationRejected('5sec', DURATION_ERROR);
  ExpectDurationRejected('1.5s', DURATION_ERROR);
  ExpectDurationRejected('-5s', DURATION_ERROR);
  ExpectDurationRejected('', DURATION_ERROR);
  ExpectDurationRejected('999999999m', TOO_LARGE_ERROR);
end;

procedure TCLIUnitsTests.TestCounts;
var
  Count: Int64;
  ErrorText: string;
begin
  Expect<Boolean>(TryParseNonNegativeCount('0', Count, ErrorText)).ToBe(True);
  Expect<Int64>(Count).ToBe(0);
  Expect<Boolean>(TryParseNonNegativeCount('2900', Count, ErrorText))
    .ToBe(True);
  Expect<Int64>(Count).ToBe(2900);
  Expect<Boolean>(TryParseNonNegativeCount('-1', Count, ErrorText))
    .ToBe(False);
  Expect<string>(ErrorText).ToBe(COUNT_ERROR);
  Expect<Boolean>(TryParseNonNegativeCount('10k', Count, ErrorText))
    .ToBe(False);
  Expect<Boolean>(TryParseNonNegativeCount('99999999999999999999', Count,
    ErrorText)).ToBe(False);
  Expect<string>(ErrorText).ToBe(TOO_LARGE_ERROR);
end;

begin
  TestRunnerProgram.AddSuite(TCLIUnitsTests.Create('CLI Units'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
