program PESafeSEH.Test;

{$I Shared.inc}

uses
  SysUtils,

  PESafeSEH,
  TestingPascalLibrary;

const
  PE_OFFSET = $80;
  OPTIONAL_OFFSET = PE_OFFSET + 24;
  SECTION_OFFSET = OPTIONAL_OFFSET + 224;
  DATA_SECTION = SECTION_OFFSET + 40;
  NEW_SECTION = DATA_SECTION + 40;
  HEADERS_SIZE = $200;
  CODE_OFFSET = $200;
  DATA_OFFSET = $400;
  ORIGINAL_SIZE = $600;
  CODE_RVA = $1000;
  DATA_RVA = $2000;
  CONFIG_RVA = $3000;
  IMAGE_BASE = $10000000; // Deliberately not the default linker base.
  DIRECTORY_OFFSET = OPTIONAL_OFFSET + 96;
  CONFIG_DIRECTORY = DIRECTORY_OFFSET + 10 * 8;
  TABLE_OFFSET = ORIGINAL_SIZE + 72;
  DEBUG_SYMBOL_COUNT = 4;
  SYMBOL_SIZE = 18;
  DEBUG_STRINGS = ORIGINAL_SIZE + DEBUG_SYMBOL_COUNT * SYMBOL_SIZE;
  DEBUG_NAMES: AnsiString = '.debug_info' + #0 + 'long_symbol_name' + #0;

type
  TPESafeSEHTests = class(TTestSuite)
  private
    procedure AssertRejected(const AImage: TBytes; const AHandlers: TSERvaArray;
      const ALabel: string; const AValidate: Boolean = False);
    procedure TestValidTable;
    procedure TestPreservesInputsAndSections;
    procedure TestHandlerRefusals;
    procedure TestMalformedHeaders;
    procedure TestUnsupportedDirectories;
    procedure TestSupportedDirectories;
    procedure TestSectionRefusals;
    procedure TestHeaderRoom;
    procedure TestOverflow;
    procedure TestValidationCorruption;
    procedure TestLargeTable;
    procedure TestEveryTruncation;
    procedure TestUnbackedSections;
    procedure TestCOFFMetadata;
    procedure TestCOFFRefusals;
    procedure TestLongSectionNames;
    procedure TestCOFFTruncation;
    procedure TestCOFFValidation;
  public
    procedure SetupTests; override;
  end;

procedure PutWord(var AImage: TBytes; const AOffset: Integer; const AValue: Word);
begin
  AImage[AOffset] := Byte(AValue and $FF);
  AImage[AOffset + 1] := Byte(AValue shr 8);
end;

procedure PutCardinal(var AImage: TBytes; const AOffset: Integer; const AValue: Cardinal);
var
  Index: Integer;
begin
  for Index := 0 to SizeOf(Cardinal) - 1 do
    AImage[AOffset + Index] := Byte((AValue shr (Index * 8)) and $FF);
end;

function GetCardinal(const AImage: TBytes; const AOffset: Integer): Cardinal;
begin
  Result := Cardinal(AImage[AOffset]) or (Cardinal(AImage[AOffset + 1]) shl 8) or
    (Cardinal(AImage[AOffset + 2]) shl 16) or (Cardinal(AImage[AOffset + 3]) shl 24);
end;

procedure PutName(var AImage: TBytes; const AOffset: Integer; const AName: AnsiString);
var
  Index: Integer;
begin
  for Index := 1 to Length(AName) do
    AImage[AOffset + Index - 1] := Ord(AName[Index]);
end;

function Fixture: TBytes;
var
  Index: Integer;
begin
  Result := nil;
  SetLength(Result, ORIGINAL_SIZE);
  PutWord(Result, 0, $5A4D);
  PutCardinal(Result, 60, PE_OFFSET);
  PutCardinal(Result, PE_OFFSET, $4550);
  PutWord(Result, PE_OFFSET + 4, $14C);
  PutWord(Result, PE_OFFSET + 6, 2);
  PutWord(Result, PE_OFFSET + 20, 224);
  PutWord(Result, PE_OFFSET + 22, $0103);
  PutWord(Result, OPTIONAL_OFFSET, $10B);
  PutCardinal(Result, OPTIONAL_OFFSET + 4, $200);
  PutCardinal(Result, OPTIONAL_OFFSET + 8, $200);
  PutCardinal(Result, OPTIONAL_OFFSET + 16, CODE_RVA);
  PutCardinal(Result, OPTIONAL_OFFSET + 28, IMAGE_BASE);
  PutCardinal(Result, OPTIONAL_OFFSET + 32, $1000);
  PutCardinal(Result, OPTIONAL_OFFSET + 36, $200);
  PutCardinal(Result, OPTIONAL_OFFSET + 56, $3000);
  PutCardinal(Result, OPTIONAL_OFFSET + 60, HEADERS_SIZE);
  PutCardinal(Result, OPTIONAL_OFFSET + 64, $12345678);
  PutWord(Result, OPTIONAL_OFFSET + 68, 3);
  PutWord(Result, OPTIONAL_OFFSET + 70, $8100);
  PutCardinal(Result, OPTIONAL_OFFSET + 92, 16);
  PutName(Result, SECTION_OFFSET, '.text');
  PutCardinal(Result, SECTION_OFFSET + 8, $180);
  PutCardinal(Result, SECTION_OFFSET + 12, CODE_RVA);
  PutCardinal(Result, SECTION_OFFSET + 16, $200);
  PutCardinal(Result, SECTION_OFFSET + 20, CODE_OFFSET);
  PutCardinal(Result, SECTION_OFFSET + 36, $60000020);
  PutName(Result, DATA_SECTION, '.data');
  PutCardinal(Result, DATA_SECTION + 8, $180);
  PutCardinal(Result, DATA_SECTION + 12, DATA_RVA);
  PutCardinal(Result, DATA_SECTION + 16, $200);
  PutCardinal(Result, DATA_SECTION + 20, DATA_OFFSET);
  PutCardinal(Result, DATA_SECTION + 36, $C0000040);
  for Index := CODE_OFFSET to High(Result) do
    Result[Index] := Byte(Index mod 251);
end;

function DebugFixture: TBytes;
var
  Index, Offset: Integer;
begin
  Result := Fixture;
  SetLength(Result, DEBUG_STRINGS + SizeOf(Cardinal) + Length(DEBUG_NAMES));
  PutCardinal(Result, PE_OFFSET + 12, ORIGINAL_SIZE);
  PutCardinal(Result, PE_OFFSET + 16, DEBUG_SYMBOL_COUNT);
  for Index := 0 to 7 do
    Result[DATA_SECTION + Index] := 0;
  PutName(Result, DATA_SECTION, '/4');
  PutCardinal(Result, DATA_SECTION + 36, $42000040);
  PutCardinal(Result, DEBUG_STRINGS, SizeOf(Cardinal) + Length(DEBUG_NAMES));
  PutName(Result, DEBUG_STRINGS + SizeOf(Cardinal), DEBUG_NAMES);
  for Index := 0 to DEBUG_SYMBOL_COUNT - 1 do
  begin
    Offset := ORIGINAL_SIZE + Index * SYMBOL_SIZE;
    PutName(Result, Offset, 'symbol');
    Result[Offset + 16] := 2;
  end;
  PutWord(Result, ORIGINAL_SIZE + 12, 1);
  // A long symbol name, an absolute symbol and an undefined symbol.
  PutCardinal(Result, ORIGINAL_SIZE + SYMBOL_SIZE, 0);
  PutCardinal(Result, ORIGINAL_SIZE + SYMBOL_SIZE + 4, 16);
  PutWord(Result, ORIGINAL_SIZE + SYMBOL_SIZE + 12, 2);
  PutCardinal(Result, ORIGINAL_SIZE + 2 * SYMBOL_SIZE + 8, $FFFFFFFF);
  PutWord(Result, ORIGINAL_SIZE + 2 * SYMBOL_SIZE + 12, $FFFF);
end;

function Handlers: TSERvaArray;
begin
  Result := TSERvaArray.Create(CODE_RVA + $70, CODE_RVA, CODE_RVA + $20);
end;

procedure TPESafeSEHTests.SetupTests;
begin
  Test('emits canonical load configuration and exact sorted table', TestValidTable);
  Test('preserves all original section bytes and both caller arrays', TestPreservesInputsAndSections);
  Test('rejects empty, duplicate, unbacked and nonexecutable handlers', TestHandlerRefusals);
  Test('rejects malformed and unsupported headers', TestMalformedHeaders);
  Test('rejects every unsupported nonzero directory', TestUnsupportedDirectories);
  Test('accepts bounded supported directories and rejects unsafe ranges', TestSupportedDirectories);
  Test('rejects overlapping, truncated and unsupported section layouts', TestSectionRefusals);
  Test('requires unused zero section header room', TestHeaderRoom);
  Test('rejects file, virtual address and initialized data overflow', TestOverflow);
  Test('validator rejects corrupted metadata and missing or extra handlers', TestValidationCorruption);
  Test('sorts a large table and grows across file and page alignment', TestLargeTable);
  Test('every truncated fixture fails with ESafeSEH without mutation', TestEveryTruncation);
  Test('accepts zero-filled BSS but never handlers in it', TestUnbackedSections);
  Test('preserves DWARF sections and moves the exact COFF tail', TestCOFFMetadata);
  Test('rejects malformed or unsupported COFF records and overlays', TestCOFFRefusals);
  Test('validates decimal long section names and their string boundaries', TestLongSectionNames);
  Test('rejects every truncated COFF tail', TestCOFFTruncation);
  Test('validator checks relocated COFF metadata', TestCOFFValidation);
end;

procedure TPESafeSEHTests.AssertRejected(const AImage: TBytes;
  const AHandlers: TSERvaArray; const ALabel: string; const AValidate: Boolean);
var
  Original, Output: TBytes;
  OriginalHandlers: TSERvaArray;
  Rejected: Boolean;
  Index: Integer;
begin
  Original := Copy(AImage, 0, Length(AImage));
  OriginalHandlers := Copy(AHandlers, 0, Length(AHandlers));
  Rejected := False;
  try
    if AValidate then
      ValidateSafeSEH(AImage, AHandlers)
    else
      Output := AddSafeSEH(AImage, AHandlers);
  except
    on E: ESafeSEH do
      Rejected := True;
  end;
  if not Rejected then
    raise ETestAssertionError.Create(UTF8Encode('Expected ESafeSEH: ' + ALabel));
  Expect<Integer>(Length(AImage)).ToBe(Length(Original));
  for Index := 0 to High(Original) do
    Expect<Byte>(AImage[Index]).ToBe(Original[Index]);
  for Index := 0 to High(OriginalHandlers) do
    Expect<Cardinal>(AHandlers[Index]).ToBe(OriginalHandlers[Index]);
end;

procedure TPESafeSEHTests.TestValidTable;
var
  Output: TBytes;
begin
  Output := AddSafeSEH(Fixture, Handlers);
  ValidateSafeSEH(Output, TSERvaArray.Create(CODE_RVA, CODE_RVA + $20, CODE_RVA + $70));
  Expect<Integer>(Length(Output)).ToBe($800);
  Expect<Byte>(Output[PE_OFFSET + 6]).ToBe(3);
  Expect<Cardinal>(GetCardinal(Output, OPTIONAL_OFFSET + 8)).ToBe($400);
  Expect<Cardinal>(GetCardinal(Output, OPTIONAL_OFFSET + 56)).ToBe($4000);
  Expect<Cardinal>(GetCardinal(Output, OPTIONAL_OFFSET + 64)).ToBe(0);
  Expect<Cardinal>(GetCardinal(Output, CONFIG_DIRECTORY)).ToBe(CONFIG_RVA);
  Expect<Cardinal>(GetCardinal(Output, CONFIG_DIRECTORY + 4)).ToBe(72);
  Expect<Cardinal>(GetCardinal(Output, NEW_SECTION + 8)).ToBe(84);
  Expect<Cardinal>(GetCardinal(Output, NEW_SECTION + 36)).ToBe($40000040);
  Expect<Cardinal>(GetCardinal(Output, ORIGINAL_SIZE)).ToBe(72);
  Expect<Cardinal>(GetCardinal(Output, ORIGINAL_SIZE + 64)).ToBe(IMAGE_BASE + CONFIG_RVA + 72);
  Expect<Cardinal>(GetCardinal(Output, ORIGINAL_SIZE + 68)).ToBe(3);
  Expect<Cardinal>(GetCardinal(Output, TABLE_OFFSET)).ToBe(CODE_RVA);
  Expect<Cardinal>(GetCardinal(Output, TABLE_OFFSET + 4)).ToBe(CODE_RVA + $20);
  Expect<Cardinal>(GetCardinal(Output, TABLE_OFFSET + 8)).ToBe(CODE_RVA + $70);
  AssertRejected(Output, Handlers, 'second edit');
  AssertRejected(Fixture, Handlers, 'missing table', True);
  Output := AddSafeSEH(Fixture, TSERvaArray.Create(CODE_RVA));
  ValidateSafeSEH(Output, TSERvaArray.Create(CODE_RVA));
  Expect<Cardinal>(GetCardinal(Output, ORIGINAL_SIZE + 68)).ToBe(1);
end;

procedure TPESafeSEHTests.TestPreservesInputsAndSections;
var
  Input, Original, Output: TBytes;
  Values: TSERvaArray;
  Index: Integer;
begin
  Input := Fixture;
  Original := Copy(Input, 0, Length(Input));
  Values := Handlers;
  Output := AddSafeSEH(Input, Values);
  for Index := 0 to High(Input) do
    Expect<Byte>(Input[Index]).ToBe(Original[Index]);
  for Index := CODE_OFFSET to High(Input) do
    Expect<Byte>(Output[Index]).ToBe(Original[Index]);
  Expect<Cardinal>(Values[0]).ToBe(CODE_RVA + $70);
  Expect<Cardinal>(Values[1]).ToBe(CODE_RVA);
  Output[CODE_OFFSET] := Output[CODE_OFFSET] xor $FF;
  Expect<Byte>(Input[CODE_OFFSET]).ToBe(Original[CODE_OFFSET]);
end;

procedure TPESafeSEHTests.TestHandlerRefusals;
const
  INVALID_RVAS: array[0..7] of Cardinal =
    (0, $100, CODE_RVA - 1, CODE_RVA + $180, CODE_RVA + $200, DATA_RVA, $3000, $FFFFFFFF);
var
  Index: Integer;
  Input: TBytes;
begin
  AssertRejected(Fixture, nil, 'empty handlers');
  AssertRejected(Fixture, TSERvaArray.Create(CODE_RVA, CODE_RVA), 'duplicate handlers');
  for Index := 0 to High(INVALID_RVAS) do
    AssertRejected(Fixture, TSERvaArray.Create(INVALID_RVAS[Index]), 'invalid handler');
  Input := Fixture;
  PutCardinal(Input, SECTION_OFFSET + 8, $400);
  AssertRejected(Input, TSERvaArray.Create(CODE_RVA + $200), 'virtual zero fill');
  Input := Fixture;
  PutCardinal(Input, SECTION_OFFSET + 36, $62000020);
  AssertRejected(Input, Handlers, 'discardable code');
end;

procedure TPESafeSEHTests.TestMalformedHeaders;
const
  OFFSETS: array[0..24] of Integer = (0, 60, 60, PE_OFFSET, PE_OFFSET + 4,
    PE_OFFSET + 6, PE_OFFSET + 6, PE_OFFSET + 20, PE_OFFSET + 22, PE_OFFSET + 22,
    OPTIONAL_OFFSET, OPTIONAL_OFFSET + 28, OPTIONAL_OFFSET + 28,
    OPTIONAL_OFFSET + 32, OPTIONAL_OFFSET + 36, OPTIONAL_OFFSET + 56,
    OPTIONAL_OFFSET + 60, OPTIONAL_OFFSET + 68, OPTIONAL_OFFSET + 70,
    OPTIONAL_OFFSET + 70, OPTIONAL_OFFSET + 92, PE_OFFSET + 12, PE_OFFSET + 16,
    OPTIONAL_OFFSET + 52, OPTIONAL_OFFSET + 88);
  VALUES: array[0..24] of Cardinal = (0, $FFFFFFFC, 0, 0, $8664,
    0, 97, 223, $2103, $0102, $20B, 0, $10000001,
    $200, $1000, $3001, $201, 10, $8140, $8500, 15, 1, 1, 1, 1);
  WORD_FIELDS: set of Byte = [0, 4, 5, 6, 7, 8, 9, 10, 17, 18, 19];
var
  Input: TBytes;
  Index: Integer;
begin
  for Index := 0 to High(OFFSETS) do
  begin
    Input := Fixture;
    if Index in WORD_FIELDS then
      PutWord(Input, OFFSETS[Index], Word(VALUES[Index]))
    else
      PutCardinal(Input, OFFSETS[Index], VALUES[Index]);
    AssertRejected(Input, Handlers, 'malformed header ' + UnicodeString(IntToStr(Index)));
  end;
end;

procedure TPESafeSEHTests.TestUnsupportedDirectories;
var
  Input: TBytes;
  Index: Integer;
begin
  for Index := 0 to 15 do
  begin
    if Index in [1, 2, 9, 12] then
      Continue;
    Input := Fixture;
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8, DATA_RVA);
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8 + 4, 16);
    AssertRejected(Input, Handlers, 'unsupported directory ' + UnicodeString(IntToStr(Index)));
  end;
end;

procedure TPESafeSEHTests.TestSupportedDirectories;
var
  Input, Output: TBytes;
  Index: Integer;
begin
  for Index := 0 to 15 do
  begin
    if not (Index in [1, 2, 9, 12]) then
      Continue;
    Input := Fixture;
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8, DATA_RVA);
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8 + 4, 16);
    Output := AddSafeSEH(Input, Handlers);
    ValidateSafeSEH(Output, Handlers);
    Expect<Cardinal>(GetCardinal(Output, DIRECTORY_OFFSET + Index * 8)).ToBe(DATA_RVA);
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8 + 4, 0);
    AssertRejected(Input, Handlers, 'missing directory size');
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8 + 4, $181);
    AssertRejected(Input, Handlers, 'directory exceeds virtual bytes');
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8, $FFFFFFF0);
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8 + 4, $100);
    AssertRejected(Input, Handlers, 'directory address overflow');
    PutCardinal(Input, DIRECTORY_OFFSET + Index * 8, 0);
    AssertRejected(Input, Handlers, 'missing directory RVA');
  end;
end;

procedure TPESafeSEHTests.TestSectionRefusals;
const
  OFFSETS: array[0..13] of Integer = (DATA_SECTION + 12, DATA_SECTION + 20,
    SECTION_OFFSET + 20, SECTION_OFFSET + 16, DATA_SECTION + 12, DATA_SECTION + 20,
    SECTION_OFFSET + 8, SECTION_OFFSET + 24, SECTION_OFFSET + 28,
    SECTION_OFFSET + 32, SECTION_OFFSET + 34, SECTION_OFFSET + 36,
    OPTIONAL_OFFSET + 16, DATA_SECTION + 16);
  VALUES: array[0..13] of Cardinal = ($1000, $200, $100, $201, $4000, $600,
    0, 1, 1, 1, 1, $60100020, DATA_RVA, $400);
var
  Input: TBytes;
  Index: Integer;
begin
  for Index := 0 to High(OFFSETS) do
  begin
    Input := Fixture;
    if Index in [9, 10] then
      PutWord(Input, OFFSETS[Index], Word(VALUES[Index]))
    else
      PutCardinal(Input, OFFSETS[Index], VALUES[Index]);
    AssertRejected(Input, Handlers, 'bad section ' + UnicodeString(IntToStr(Index)));
  end;
  Input := Fixture;
  SetLength(Input, ORIGINAL_SIZE + $200);
  AssertRejected(Input, Handlers, 'aligned overlay');
  Input := Fixture;
  SetLength(Input, ORIGINAL_SIZE + 1);
  AssertRejected(Input, Handlers, 'unaligned file');
  Input := Fixture;
  PutName(Input, DATA_SECTION, '.loadcfg');
  AssertRejected(Input, Handlers, 'existing named load configuration');
end;

procedure TPESafeSEHTests.TestHeaderRoom;
var
  Input: TBytes;
begin
  Input := Fixture;
  Input[NEW_SECTION + 39] := 1;
  AssertRejected(Input, Handlers, 'occupied header padding');
  Input := Fixture;
  PutWord(Input, PE_OFFSET + 6, 3);
  PutCardinal(Input, OPTIONAL_OFFSET + 56, $4000);
  PutName(Input, NEW_SECTION, '.bss');
  PutCardinal(Input, NEW_SECTION + 8, $100);
  PutCardinal(Input, NEW_SECTION + 12, $3000);
  PutCardinal(Input, NEW_SECTION + 36, $C0000080);
  AssertRejected(Input, Handlers, 'no room for fourth section');
end;

procedure TPESafeSEHTests.TestOverflow;
var
  Input: TBytes;
begin
  Input := Fixture;
  PutCardinal(Input, OPTIONAL_OFFSET + 8, $FFFFFF00);
  AssertRejected(Input, Handlers, 'initialized data overflow');
  Input := Fixture;
  PutCardinal(Input, OPTIONAL_OFFSET + 28, $FFFF0000);
  PutCardinal(Input, DATA_SECTION + 8, $D000);
  PutCardinal(Input, OPTIONAL_OFFSET + 56, $F000);
  AssertRejected(Input, Handlers, 'new image VA overflow');
  Input := Fixture;
  PutCardinal(Input, DATA_SECTION + 8, $FFFFFFFF);
  AssertRejected(Input, Handlers, 'virtual size overflow');
  Input := Fixture;
  PutCardinal(Input, DATA_SECTION + 16, $FFFFFE00);
  AssertRejected(Input, Handlers, 'raw size overflow');
end;

procedure TPESafeSEHTests.TestValidationCorruption;
const
  OFFSETS: array[0..14] of Integer = (CONFIG_DIRECTORY, CONFIG_DIRECTORY + 4,
    NEW_SECTION + 36, NEW_SECTION + 36, NEW_SECTION + 8, ORIGINAL_SIZE,
    ORIGINAL_SIZE + 4, ORIGINAL_SIZE + 64, ORIGINAL_SIZE + 68,
    TABLE_OFFSET, TABLE_OFFSET + 4, TABLE_OFFSET + 8, TABLE_OFFSET + 12,
    OPTIONAL_OFFSET + 64, OPTIONAL_OFFSET + 8);
  VALUES: array[0..14] of Cardinal = (CONFIG_RVA + 4, 71, $C0000040,
    $60000040, 85, 71, 1, $FFFFFFFC, $FFFFFFFF,
    CODE_RVA + $70, CODE_RVA, DATA_RVA, 1, 1, 0);
var
  Output, Corrupt: TBytes;
  Index: Integer;
begin
  Output := AddSafeSEH(Fixture, Handlers);
  for Index := 0 to High(OFFSETS) do
  begin
    Corrupt := Copy(Output, 0, Length(Output));
    PutCardinal(Corrupt, OFFSETS[Index], VALUES[Index]);
    AssertRejected(Corrupt, Handlers, 'corrupt table ' + UnicodeString(IntToStr(Index)), True);
  end;
  AssertRejected(Output, TSERvaArray.Create(CODE_RVA), 'missing supplied handlers', True);
  AssertRejected(Output, TSERvaArray.Create(CODE_RVA, CODE_RVA + 1, CODE_RVA + $20, CODE_RVA + $70),
    'extra supplied handler', True);
  AssertRejected(Output, TSERvaArray.Create(CODE_RVA, CODE_RVA, CODE_RVA + $20), 'duplicate supplied handler', True);
  AssertRejected(Output, TSERvaArray.Create(CODE_RVA, CODE_RVA + $20, CODE_RVA + $71), 'different supplied handler', True);
  AssertRejected(Output, nil, 'empty supplied handlers', True);
end;

procedure TPESafeSEHTests.TestLargeTable;
const
  HANDLER_COUNT = 1100;
  LARGE_CODE_SIZE = $2000;
  LARGE_ORIGINAL_SIZE = HEADERS_SIZE + LARGE_CODE_SIZE + $200;
var
  Input, Output: TBytes;
  Values: TSERvaArray;
  Index: Integer;
begin
  Input := Fixture;
  SetLength(Input, LARGE_ORIGINAL_SIZE);
  PutCardinal(Input, SECTION_OFFSET + 8, LARGE_CODE_SIZE);
  PutCardinal(Input, SECTION_OFFSET + 16, LARGE_CODE_SIZE);
  PutCardinal(Input, DATA_SECTION + 12, $3000);
  PutCardinal(Input, DATA_SECTION + 20, HEADERS_SIZE + LARGE_CODE_SIZE);
  PutCardinal(Input, OPTIONAL_OFFSET + 56, $4000);
  SetLength(Values, HANDLER_COUNT);
  for Index := 0 to High(Values) do
    Values[Index] := CODE_RVA + Cardinal(HANDLER_COUNT - Index - 1);
  Output := AddSafeSEH(Input, Values);
  ValidateSafeSEH(Output, Values);
  Expect<Integer>(Length(Output)).ToBe(LARGE_ORIGINAL_SIZE + $1200);
  Expect<Cardinal>(GetCardinal(Output, OPTIONAL_OFFSET + 56)).ToBe($6000);
  for Index := 0 to HANDLER_COUNT - 1 do
    Expect<Cardinal>(GetCardinal(Output, LARGE_ORIGINAL_SIZE + 72 + Index * 4)).ToBe(CODE_RVA + Cardinal(Index));
end;

procedure TPESafeSEHTests.TestEveryTruncation;
var
  Input: TBytes;
  Index: Integer;
begin
  for Index := 0 to ORIGINAL_SIZE - 1 do
  begin
    Input := Fixture;
    SetLength(Input, Index);
    AssertRejected(Input, Handlers, 'truncated at ' + UnicodeString(IntToStr(Index)));
  end;
end;

procedure TPESafeSEHTests.TestUnbackedSections;
var
  Input, Output: TBytes;
begin
  Input := Fixture;
  SetLength(Input, DATA_OFFSET);
  PutName(Input, DATA_SECTION, '.bss' + #0);
  PutCardinal(Input, DATA_SECTION + 16, 0);
  PutCardinal(Input, DATA_SECTION + 20, 0);
  PutCardinal(Input, DATA_SECTION + 36, $C0000080);
  Output := AddSafeSEH(Input, Handlers);
  ValidateSafeSEH(Output, Handlers);
  Expect<Integer>(Length(Output)).ToBe(ORIGINAL_SIZE);
  AssertRejected(Input, TSERvaArray.Create(DATA_RVA), 'BSS handler');
  PutCardinal(Input, DATA_SECTION + 20, DATA_OFFSET);
  AssertRejected(Input, Handlers, 'BSS raw pointer');
end;

procedure TPESafeSEHTests.TestCOFFMetadata;
var
  Input, Original, Output: TBytes;
  Index: Integer;
begin
  Input := DebugFixture;
  Original := Copy(Input, 0, Length(Input));
  Output := AddSafeSEH(Input, Handlers);
  ValidateSafeSEH(Output, Handlers);
  Expect<Integer>(Length(Output)).ToBe(Length(Input) + $200);
  Expect<Cardinal>(GetCardinal(Output, PE_OFFSET + 12)).ToBe(ORIGINAL_SIZE + $200);
  Expect<Cardinal>(GetCardinal(Output, PE_OFFSET + 16)).ToBe(DEBUG_SYMBOL_COUNT);
  Expect<Cardinal>(GetCardinal(Output, NEW_SECTION + 20)).ToBe(ORIGINAL_SIZE);
  for Index := 0 to High(Input) do
    Expect<Byte>(Input[Index]).ToBe(Original[Index]);
  for Index := SECTION_OFFSET to NEW_SECTION - 1 do
    Expect<Byte>(Output[Index]).ToBe(Input[Index]);
  for Index := CODE_OFFSET to ORIGINAL_SIZE - 1 do
    Expect<Byte>(Output[Index]).ToBe(Input[Index]);
  for Index := ORIGINAL_SIZE to High(Input) do
    Expect<Byte>(Output[Index + $200]).ToBe(Input[Index]);
  // Long section and symbol names still use the same string-relative offsets.
  Expect<Byte>(Output[DATA_SECTION]).ToBe(Ord('/'));
  Expect<Cardinal>(GetCardinal(Output, ORIGINAL_SIZE + $200 + SYMBOL_SIZE + 4)).ToBe(16);
  Expect<Cardinal>(GetCardinal(Output, ORIGINAL_SIZE + 64)).ToBe(IMAGE_BASE + CONFIG_RVA + 72);
end;

procedure TPESafeSEHTests.TestCOFFRefusals;
const
  OFFSETS: array[0..16] of Integer = (PE_OFFSET + 12, PE_OFFSET + 12,
    PE_OFFSET + 12, PE_OFFSET + 16, DEBUG_STRINGS, DEBUG_STRINGS,
    DEBUG_STRINGS, ORIGINAL_SIZE + SYMBOL_SIZE + 4, ORIGINAL_SIZE + SYMBOL_SIZE + 4,
    ORIGINAL_SIZE + SYMBOL_SIZE + 4, ORIGINAL_SIZE + 12, ORIGINAL_SIZE + 12,
    ORIGINAL_SIZE + 8, ORIGINAL_SIZE + 3 * SYMBOL_SIZE + 8,
    ORIGINAL_SIZE + 14, ORIGINAL_SIZE + 16, ORIGINAL_SIZE + 17);
  VALUES: array[0..16] of Cardinal = (0, ORIGINAL_SIZE - $200,
    ORIGINAL_SIZE + 1, $FFFFFFFF, 3, 4, $FFFFFFFF, 0, 5, $FFFFFFFF,
    3, $FFFE, $181, 1, 1, 3, 1);
var
  Input: TBytes;
  Index: Integer;
begin
  for Index := 0 to High(OFFSETS) do
  begin
    Input := DebugFixture;
    if Index in [10, 11, 14] then
      PutWord(Input, OFFSETS[Index], Word(VALUES[Index]))
    else if Index in [15, 16] then
      Input[OFFSETS[Index]] := Byte(VALUES[Index])
    else
      PutCardinal(Input, OFFSETS[Index], VALUES[Index]);
    AssertRejected(Input, Handlers, 'malformed COFF ' + UnicodeString(IntToStr(Index)));
  end;
  Input := DebugFixture;
  Input[High(Input)] := Ord('x');
  AssertRejected(Input, Handlers, 'unterminated string table');
  Input := DebugFixture;
  PutCardinal(Input, ORIGINAL_SIZE + SYMBOL_SIZE + 4, 15);
  AssertRejected(Input, Handlers, 'name points to terminating NUL');
  Input := DebugFixture;
  Input[ORIGINAL_SIZE + 7] := 1;
  AssertRejected(Input, Handlers, 'nonzero inline name padding');
  Input := DebugFixture;
  SetLength(Input, Length(Input) + 1);
  AssertRejected(Input, Handlers, 'overlay after COFF metadata');
  Input := DebugFixture;
  PutCardinal(Input, DIRECTORY_OFFSET + 4 * 8, ORIGINAL_SIZE);
  PutCardinal(Input, DIRECTORY_OFFSET + 4 * 8 + 4, 16);
  AssertRejected(Input, Handlers, 'certificate in debug image');
end;

procedure TPESafeSEHTests.TestLongSectionNames;
const
  INVALID_NAMES: array[0..6] of AnsiString = ('/', '/x', '/4x', '/3', '/5', '/15', '/9999999');
var
  Input: TBytes;
  Index, NameIndex: Integer;
begin
  for NameIndex := 0 to High(INVALID_NAMES) do
  begin
    Input := DebugFixture;
    for Index := 0 to 7 do
      Input[DATA_SECTION + Index] := 0;
    PutName(Input, DATA_SECTION, INVALID_NAMES[NameIndex]);
    AssertRejected(Input, Handlers, 'bad long section name');
  end;
  Input := Fixture;
  for Index := 0 to 7 do
    Input[DATA_SECTION + Index] := 0;
  PutName(Input, DATA_SECTION, '/4');
  AssertRejected(Input, Handlers, 'long section name without COFF strings');
  Input := DebugFixture;
  PutName(Input, DEBUG_STRINGS + 4, '.loadcfg' + #0);
  AssertRejected(Input, Handlers, 'existing .loadcfg through a string reference');
  Input := DebugFixture;
  Input[DATA_SECTION + 7] := Ord('x');
  AssertRejected(Input, Handlers, 'nonzero long name padding');
end;

procedure TPESafeSEHTests.TestCOFFTruncation;
var
  Input: TBytes;
  Index, TotalSize: Integer;
begin
  Input := DebugFixture;
  TotalSize := Length(Input);
  for Index := ORIGINAL_SIZE to TotalSize - 1 do
  begin
    Input := DebugFixture;
    SetLength(Input, Index);
    AssertRejected(Input, Handlers, 'truncated COFF tail');
  end;
end;

procedure TPESafeSEHTests.TestCOFFValidation;
var
  Output: TBytes;
begin
  Output := AddSafeSEH(DebugFixture, Handlers);
  PutCardinal(Output, PE_OFFSET + 12, ORIGINAL_SIZE);
  AssertRejected(Output, Handlers, 'stale symbol table pointer', True);
  Output := AddSafeSEH(DebugFixture, Handlers);
  PutCardinal(Output, ORIGINAL_SIZE + $200 + SYMBOL_SIZE + 4, $FFFFFFFF);
  AssertRejected(Output, Handlers, 'corrupt relocated symbol name', True);
  Output := AddSafeSEH(DebugFixture, Handlers);
  PutWord(Output, ORIGINAL_SIZE + $200 + 12, 3);
  AssertRejected(Output, Handlers, 'old symbol references new section', True);
end;

begin
  TestRunnerProgram.AddSuite(TPESafeSEHTests.Create('PESafeSEH'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
