program EmbeddedResourceReader.Test;

{$I Shared.inc}

uses
  SysUtils,

  EmbeddedResourceReader,
  TestingPascalLibrary;

type
  TEmbeddedResourceReaderTests = class(TTestSuite)
  private
    function CreateContainer: TBytes;
    procedure WriteUInt32LE(var ABuffer: TBytes; const AValue: UInt32;
      const AOffset: Integer);
    procedure CopyAscii(const AText: string; var ABuffer: TBytes;
      const AOffset: Integer);

    procedure TestReadsContainerAndFindsEntry;
    procedure TestMissingEntryReturnsFalse;
    procedure TestWrongMagicIsRejected;
    procedure TestWrongFormatVersionIsRejected;
    procedure TestOutOfRangeEntryDataIsRejected;
  public
    procedure SetupTests; override;
  end;

const
  TEST_MAGIC: TEmbeddedResourceMagic =
    (Ord('T'), Ord('E'), Ord('S'), Ord('T'), Ord('D'), Ord('A'), Ord('T'), Ord('A'));
  HEADER_SIZE = 32;
  VERSION_OFFSET = 8;
  VERSION_LENGTH = 2;
  ENTRY_TABLE_OFFSET = HEADER_SIZE + VERSION_LENGTH;
  ENTRY_SIZE = 16;
  NAME_SECTION_OFFSET = ENTRY_TABLE_OFFSET + 2 * ENTRY_SIZE;
  DATA_SECTION_OFFSET = NAME_SECTION_OFFSET + 11;
  TOTAL_SIZE = DATA_SECTION_OFFSET + 9;

procedure TEmbeddedResourceReaderTests.SetupTests;
begin
  Test('Reads container metadata and finds entry data', TestReadsContainerAndFindsEntry);
  Test('Missing entry returns false', TestMissingEntryReturnsFalse);
  Test('Wrong magic is rejected', TestWrongMagicIsRejected);
  Test('Wrong format version is rejected', TestWrongFormatVersionIsRejected);
  Test('Out-of-range entry data is rejected', TestOutOfRangeEntryDataIsRejected);
end;

procedure TEmbeddedResourceReaderTests.WriteUInt32LE(var ABuffer: TBytes;
  const AValue: UInt32; const AOffset: Integer);
begin
  ABuffer[AOffset] := Byte(AValue and $ff);
  ABuffer[AOffset + 1] := Byte((AValue shr 8) and $ff);
  ABuffer[AOffset + 2] := Byte((AValue shr 16) and $ff);
  ABuffer[AOffset + 3] := Byte((AValue shr 24) and $ff);
end;

procedure TEmbeddedResourceReaderTests.CopyAscii(const AText: string;
  var ABuffer: TBytes; const AOffset: Integer);
var
  Index: Integer;
begin
  for Index := 1 to Length(AText) do
    ABuffer[AOffset + Index - 1] := Byte(AText[Index]);
end;

function TEmbeddedResourceReaderTests.CreateContainer: TBytes;
var
  Index: Integer;
begin
  SetLength(Result, TOTAL_SIZE);
  for Index := 0 to Length(TEST_MAGIC) - 1 do
    Result[Index] := TEST_MAGIC[Index];

  WriteUInt32LE(Result, 1, VERSION_OFFSET);
  WriteUInt32LE(Result, VERSION_LENGTH, VERSION_OFFSET + 4);
  WriteUInt32LE(Result, 2, VERSION_OFFSET + 8);
  WriteUInt32LE(Result, 11, VERSION_OFFSET + 12);
  WriteUInt32LE(Result, 9, VERSION_OFFSET + 16);
  WriteUInt32LE(Result, 0, VERSION_OFFSET + 20);

  CopyAscii('v1', Result, HEADER_SIZE);

  WriteUInt32LE(Result, 0, ENTRY_TABLE_OFFSET);
  WriteUInt32LE(Result, 5, ENTRY_TABLE_OFFSET + 4);
  WriteUInt32LE(Result, 0, ENTRY_TABLE_OFFSET + 8);
  WriteUInt32LE(Result, 3, ENTRY_TABLE_OFFSET + 12);

  WriteUInt32LE(Result, 5, ENTRY_TABLE_OFFSET + ENTRY_SIZE);
  WriteUInt32LE(Result, 6, ENTRY_TABLE_OFFSET + ENTRY_SIZE + 4);
  WriteUInt32LE(Result, 3, ENTRY_TABLE_OFFSET + ENTRY_SIZE + 8);
  WriteUInt32LE(Result, 6, ENTRY_TABLE_OFFSET + ENTRY_SIZE + 12);

  CopyAscii('applebanana', Result, NAME_SECTION_OFFSET);
  CopyAscii('redyellow', Result, DATA_SECTION_OFFSET);
end;

procedure TEmbeddedResourceReaderTests.TestReadsContainerAndFindsEntry;
var
  Buffer, Bytes: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
  DataOffset, DataLength: Integer;
begin
  Buffer := CreateContainer;

  Expect<Boolean>(TryReadEmbeddedResourceContainer(Buffer, TEST_MAGIC, Container)).ToBe(True);
  Expect<Integer>(Container.EntryCount).ToBe(2);
  Expect<Integer>(Container.EntryTableOffset).ToBe(ENTRY_TABLE_OFFSET);
  Expect<Integer>(Container.NamesOffset).ToBe(NAME_SECTION_OFFSET);
  Expect<Integer>(Container.DataOffset).ToBe(DATA_SECTION_OFFSET);

  Expect<Boolean>(TryFindEmbeddedResourceEntry(Buffer, 'banana', Container, Entry)).ToBe(True);
  Expect<Boolean>(TryGetEmbeddedResourceEntryDataBounds(Buffer, Container, Entry,
    DataOffset, DataLength)).ToBe(True);
  Expect<string>(CopyStringFromBytes(Buffer, DataOffset, DataLength)).ToBe('yellow');

  Expect<Boolean>(TryFindEmbeddedResourceEntry(Buffer, 'apple', Container, Entry)).ToBe(True);
  Expect<Boolean>(TryCopyEmbeddedResourceEntryData(Buffer, Container, Entry, Bytes)).ToBe(True);
  Expect<string>(CopyStringFromBytes(Bytes, 0, Length(Bytes))).ToBe('red');
end;

procedure TEmbeddedResourceReaderTests.TestMissingEntryReturnsFalse;
var
  Buffer: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
begin
  Buffer := CreateContainer;

  Expect<Boolean>(TryReadEmbeddedResourceContainer(Buffer, TEST_MAGIC, Container)).ToBe(True);
  Expect<Boolean>(TryFindEmbeddedResourceEntry(Buffer, 'cherry', Container, Entry)).ToBe(False);
end;

procedure TEmbeddedResourceReaderTests.TestWrongMagicIsRejected;
var
  Buffer: TBytes;
  Container: TEmbeddedResourceContainer;
begin
  Buffer := CreateContainer;
  Buffer[0] := Ord('X');

  Expect<Boolean>(TryReadEmbeddedResourceContainer(Buffer, TEST_MAGIC, Container)).ToBe(False);
end;

procedure TEmbeddedResourceReaderTests.TestWrongFormatVersionIsRejected;
var
  Buffer: TBytes;
  Container: TEmbeddedResourceContainer;
begin
  Buffer := CreateContainer;
  WriteUInt32LE(Buffer, 2, VERSION_OFFSET);

  Expect<Boolean>(TryReadEmbeddedResourceContainer(Buffer, TEST_MAGIC, Container)).ToBe(False);
end;

procedure TEmbeddedResourceReaderTests.TestOutOfRangeEntryDataIsRejected;
var
  Buffer: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
  DataOffset, DataLength: Integer;
begin
  Buffer := CreateContainer;
  WriteUInt32LE(Buffer, 100, ENTRY_TABLE_OFFSET + 12);

  Expect<Boolean>(TryReadEmbeddedResourceContainer(Buffer, TEST_MAGIC, Container)).ToBe(True);
  Expect<Boolean>(TryFindEmbeddedResourceEntry(Buffer, 'apple', Container, Entry)).ToBe(True);
  Expect<Boolean>(TryGetEmbeddedResourceEntryDataBounds(Buffer, Container, Entry,
    DataOffset, DataLength)).ToBe(False);
end;

begin
  TestRunnerProgram.AddSuite(TEmbeddedResourceReaderTests.Create('EmbeddedResourceReader'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
