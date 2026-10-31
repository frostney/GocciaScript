unit PESafeSEH;

{$I Shared.inc}

interface

uses
  SysUtils;

type
  ESafeSEH = class(Exception);
  TSERvaArray = array of Cardinal;

// AHandlers must describe ALL handlers in this exact linked image. The caller
// owns extraction and coverage proof; executable bytes alone cannot prove that.
// Empty and duplicate lists are rejected. Neither input array is modified.
// Supported images use 4096/512 alignment, contiguous ordered sections, and
// only import, resource, TLS and IAT directories (each wholly file-backed).
// FPC debug images may end with plain external COFF symbols and their string
// table. That bounded tail moves intact; all existing section offsets stay put.
function AddSafeSEH(const AImage: TBytes; const AHandlers: TSERvaArray): TBytes;
// Accepts the canonical appended format produced by AddSafeSEH, and checks its
// exact handler set (caller order is immaterial). Raises ESafeSEH on mismatch.
procedure ValidateSafeSEH(const AImage: TBytes; const AHandlers: TSERvaArray);

implementation

const
  DOS_MAGIC = $5A4D;
  DOS_HEADER_SIZE = 64;
  DOS_PE_OFFSET = 60;
  PE_MAGIC = $00004550;
  COFF_HEADER_SIZE = 24; // Includes PE signature.
  COFF_MACHINE = 4;
  COFF_SECTION_COUNT = 6;
  COFF_SYMBOLS = 12;
  COFF_SYMBOL_COUNT = 16;
  COFF_OPTIONAL_SIZE = 20;
  COFF_CHARACTERISTICS = 22;
  COFF_SYMBOL_SIZE = 18;
  COFF_SYMBOL_NAME_SIZE = 8;
  COFF_SYMBOL_VALUE = 8;
  COFF_SYMBOL_SECTION = 12;
  COFF_SYMBOL_TYPE = 14;
  COFF_SYMBOL_STORAGE = 16;
  COFF_SYMBOL_AUX_COUNT = 17;
  COFF_STORAGE_EXTERNAL = 2;
  COFF_SECTION_ABSOLUTE = $FFFF;
  DECIMAL_RADIX = 10;
  MACHINE_I386 = $014C;
  FILE_RELOCS_STRIPPED = $0001;
  FILE_EXECUTABLE_IMAGE = $0002;
  FILE_DLL = $2000;
  PE32_MAGIC = $010B;
  OPTIONAL_SIZE = 224;
  OPTIONAL_DATA_SIZE = 8;
  OPTIONAL_ENTRY_POINT = 16;
  OPTIONAL_IMAGE_BASE = 28;
  OPTIONAL_SECTION_ALIGNMENT = 32;
  OPTIONAL_FILE_ALIGNMENT = 36;
  OPTIONAL_WIN32_VERSION = 52;
  OPTIONAL_IMAGE_SIZE = 56;
  OPTIONAL_HEADERS_SIZE = 60;
  OPTIONAL_CHECKSUM = 64;
  OPTIONAL_SUBSYSTEM = 68;
  OPTIONAL_DLL_CHARACTERISTICS = 70;
  OPTIONAL_LOADER_FLAGS = 88;
  OPTIONAL_DIRECTORY_COUNT = 92;
  OPTIONAL_DIRECTORIES = 96;
  SUBSYSTEM_WINDOWS_GUI = 2;
  SUBSYSTEM_WINDOWS_CUI = 3;
  DLL_SUPPORTED_FLAGS = $8B00; // NX, NO_ISOLATION, NO_BIND, TERMINAL_SERVER_AWARE.
  DIRECTORY_COUNT = 16;
  DIRECTORY_SIZE = 8;
  DIRECTORY_IMPORT = 1;
  DIRECTORY_RESOURCE = 2;
  DIRECTORY_TLS = 9;
  DIRECTORY_LOAD_CONFIG = 10;
  DIRECTORY_IAT = 12;
  SECTION_SIZE = 40;
  SECTION_NAME_SIZE = 8;
  SECTION_VIRTUAL_SIZE = 8;
  SECTION_RVA = 12;
  SECTION_RAW_SIZE = 16;
  SECTION_RAW_OFFSET = 20;
  SECTION_RELOCATIONS = 24;
  SECTION_LINES = 28;
  SECTION_RELOCATION_COUNT = 32;
  SECTION_LINE_COUNT = 34;
  SECTION_CHARACTERISTICS = 36;
  SECTION_EXECUTE = $20000000;
  SECTION_DISCARDABLE = $02000000;
  SECTION_READONLY_DATA = $40000040;
  SECTION_SUPPORTED_FLAGS = $E60000E0; // Code/data, discardable, uncached, R/W/X.
  MAX_SECTIONS = 96; // Windows loader limit.
  SECTION_ALIGNMENT = 4096;
  FILE_ALIGNMENT = 512;
  IMAGE_BASE_ALIGNMENT = 65536;
  LOAD_CONFIG_SIZE = 72;
  LOAD_CONFIG_TABLE = 64;
  LOAD_CONFIG_COUNT = 68;
  HANDLER_SIZE = 4;
  LOAD_CONFIG_NAME: AnsiString = '.loadcfg';

type
  TPESection = record
    HeaderOffset: Cardinal;
    NameOffset: Cardinal;
    NameSize: Cardinal;
    VirtualSize: Cardinal;
    Rva: Cardinal;
    RawSize: Cardinal;
    RawOffset: Cardinal;
    Characteristics: Cardinal;
  end;
  TPESections = array of TPESection;
  TPEImage = record
    HeaderOffset: Cardinal;
    OptionalOffset: Cardinal;
    SectionsOffset: Cardinal;
    HeadersSize: Cardinal;
    ImageSize: Cardinal;
    ImageBase: Cardinal;
    RawEnd: Cardinal;
    SymbolsOffset: Cardinal;
    SymbolCount: Cardinal;
    StringsOffset: Cardinal;
    StringsSize: Cardinal;
    Sections: TPESections;
  end;

procedure Require(const ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise ESafeSEH.Create(UTF8Encode(AMessage));
end;

function CheckedCardinal(const AValue: UInt64): Cardinal;
begin
  Require(AValue <= High(Cardinal), 'PE32 address or size overflow');
  Result := Cardinal(AValue);
end;

function Aligned(const AValue: UInt64; const AAlignment: Cardinal): Cardinal;
begin
  Result := CheckedCardinal(((AValue + AAlignment - 1) div AAlignment) * AAlignment);
end;

procedure RequireBytes(const AImage: TBytes; const AOffset, ASize: UInt64);
begin
  Require(AOffset <= UInt64(Length(AImage)), 'Truncated PE image');
  Require(ASize <= UInt64(Length(AImage)) - AOffset, 'Truncated PE image');
end;

function ReadWord(const AImage: TBytes; const AOffset: UInt64): Word;
begin
  RequireBytes(AImage, AOffset, SizeOf(Word));
  Result := Word(AImage[AOffset]) or (Word(AImage[AOffset + 1]) shl 8);
end;

function ReadCardinal(const AImage: TBytes; const AOffset: UInt64): Cardinal;
begin
  RequireBytes(AImage, AOffset, SizeOf(Cardinal));
  Result := Cardinal(AImage[AOffset]) or (Cardinal(AImage[AOffset + 1]) shl 8) or
    (Cardinal(AImage[AOffset + 2]) shl 16) or (Cardinal(AImage[AOffset + 3]) shl 24);
end;

procedure WriteWord(var AImage: TBytes; const AOffset: Cardinal; const AValue: Word);
begin
  RequireBytes(AImage, AOffset, SizeOf(Word));
  AImage[AOffset] := Byte(AValue and $FF);
  AImage[AOffset + 1] := Byte(AValue shr 8);
end;

procedure WriteCardinal(var AImage: TBytes; const AOffset, AValue: Cardinal);
var
  Index: Integer;
begin
  RequireBytes(AImage, AOffset, SizeOf(Cardinal));
  for Index := 0 to SizeOf(Cardinal) - 1 do
    AImage[AOffset + Cardinal(Index)] := Byte((AValue shr (Index * 8)) and $FF);
end;

procedure RequireZero(const AImage: TBytes; const AOffset, ASize: Cardinal);
var
  Index: UInt64;
begin
  RequireBytes(AImage, AOffset, ASize);
  Index := AOffset;
  while Index < UInt64(AOffset) + ASize do
  begin
    Require(AImage[Index] = 0, 'Nonzero reserved bytes or header padding');
    Inc(Index);
  end;
end;

function IsLoadConfig(const AImage: TBytes; const ASection: TPESection): Boolean;
var
  Index: Integer;
begin
  if ASection.NameSize <> SECTION_NAME_SIZE then
    Exit(False);
  for Index := 1 to SECTION_NAME_SIZE do
    if AImage[ASection.NameOffset + Cardinal(Index - 1)] <> Ord(LOAD_CONFIG_NAME[Index]) then
      Exit(False);
  Result := True;
end;

// FPC's internal linker emits an unaligned COFF/string tail immediately after
// the aligned section bytes. It contains no file-offset-bearing auxiliary
// records, so moving it requires changing only PointerToSymbolTable.
procedure ParseCOFFBounds(const AImage: TBytes; var AParsed: TPEImage);
begin
  AParsed.SymbolsOffset := ReadCardinal(AImage, AParsed.HeaderOffset + COFF_SYMBOLS);
  AParsed.SymbolCount := ReadCardinal(AImage, AParsed.HeaderOffset + COFF_SYMBOL_COUNT);
  AParsed.StringsOffset := 0;
  AParsed.StringsSize := 0;
  if AParsed.SymbolsOffset = 0 then
  begin
    Require(AParsed.SymbolCount = 0, 'COFF count without symbol table');
    Exit;
  end;
  Require((AParsed.SymbolsOffset >= AParsed.HeadersSize) and
    (AParsed.SymbolsOffset mod FILE_ALIGNMENT = 0), 'Invalid COFF symbol table offset');
  RequireBytes(AImage, AParsed.SymbolsOffset, UInt64(AParsed.SymbolCount) * COFF_SYMBOL_SIZE);
  AParsed.StringsOffset := CheckedCardinal(UInt64(AParsed.SymbolsOffset) +
    UInt64(AParsed.SymbolCount) * COFF_SYMBOL_SIZE);
  AParsed.StringsSize := ReadCardinal(AImage, AParsed.StringsOffset);
  Require(AParsed.StringsSize >= SizeOf(Cardinal), 'Invalid COFF string table size');
  Require(UInt64(AParsed.StringsOffset) + AParsed.StringsSize = UInt64(Length(AImage)),
    'Truncated COFF string table or trailing overlay');
  // Every referenced start below therefore has a bounded terminating NUL.
  if AParsed.StringsSize > SizeOf(Cardinal) then
    Require(AImage[High(AImage)] = 0, 'Unterminated COFF string table');
end;

function COFFNameOffset(const AImage: TBytes; const AParsed: TPEImage;
  const ARelativeOffset: Cardinal): Cardinal;
begin
  Require((ARelativeOffset >= SizeOf(Cardinal)) and (ARelativeOffset < AParsed.StringsSize),
    'COFF name offset outside string table');
  Result := AParsed.StringsOffset + ARelativeOffset;
  Require(AImage[Result] <> 0, 'Empty COFF name');
  if ARelativeOffset > SizeOf(Cardinal) then
    Require(AImage[Result - 1] = 0, 'COFF name does not start at a string boundary');
end;

function InlineNameSize(const AImage: TBytes; const AOffset: Cardinal): Cardinal;
begin
  Result := 0;
  while (Result < COFF_SYMBOL_NAME_SIZE) and (AImage[AOffset + Result] <> 0) do
    Inc(Result);
  Require(Result > 0, 'Empty section or symbol name');
  RequireZero(AImage, AOffset + Result, COFF_SYMBOL_NAME_SIZE - Result);
end;

procedure ParseSectionName(const AImage: TBytes; const AParsed: TPEImage;
  var ASection: TPESection);
var
  Index, RelativeOffset: Cardinal;
  Digit: Byte;
begin
  ASection.NameOffset := ASection.HeaderOffset;
  ASection.NameSize := InlineNameSize(AImage, ASection.HeaderOffset);
  if AImage[ASection.HeaderOffset] <> Ord('/') then
    Exit;
  Require(ASection.NameSize > 1, 'Missing long section name offset');
  RelativeOffset := 0;
  for Index := 1 to ASection.NameSize - 1 do
  begin
    Digit := AImage[ASection.HeaderOffset + Index];
    Require((Digit >= Ord('0')) and (Digit <= Ord('9')), 'Invalid long section name offset');
    RelativeOffset := CheckedCardinal(UInt64(RelativeOffset) * DECIMAL_RADIX + Digit - Ord('0'));
  end;
  ASection.NameOffset := COFFNameOffset(AImage, AParsed, RelativeOffset);
  ASection.NameSize := 0;
  // At most MAX_SECTIONS scans, each bounded by the checked string-table end.
  while AImage[ASection.NameOffset + ASection.NameSize] <> 0 do
    Inc(ASection.NameSize);
end;

procedure ValidateCOFFSymbols(const AImage: TBytes; const AParsed: TPEImage;
  const AEdited: Boolean);
var
  Index: UInt64;
  Offset, Value: Cardinal;
  SectionNumber, SectionCount: Integer;
begin
  SectionCount := Length(AParsed.Sections);
  if AEdited then
    Dec(SectionCount);
  Index := 0;
  while Index < AParsed.SymbolCount do
  begin
    Offset := CheckedCardinal(UInt64(AParsed.SymbolsOffset) + Index * COFF_SYMBOL_SIZE);
    if ReadCardinal(AImage, Offset) = 0 then
      COFFNameOffset(AImage, AParsed, ReadCardinal(AImage, Offset + SizeOf(Cardinal)))
    else
      InlineNameSize(AImage, Offset);
    // Support the exact external, untyped, auxiliary-free form emitted by FPC.
    // Other forms can contain file pointers and need their own relocation rules.
    Require((ReadWord(AImage, Offset + COFF_SYMBOL_TYPE) = 0) and
      (AImage[Offset + COFF_SYMBOL_STORAGE] = COFF_STORAGE_EXTERNAL) and
      (AImage[Offset + COFF_SYMBOL_AUX_COUNT] = 0), 'Unsupported COFF symbol form');
    SectionNumber := ReadWord(AImage, Offset + COFF_SYMBOL_SECTION);
    Value := ReadCardinal(AImage, Offset + COFF_SYMBOL_VALUE);
    if SectionNumber = 0 then
      Require(Value = 0, 'Unsupported COFF common symbol')
    else if SectionNumber <> COFF_SECTION_ABSOLUTE then
    begin
      Require(SectionNumber <= SectionCount, 'COFF symbol section out of range');
      Require(Value <= AParsed.Sections[SectionNumber - 1].VirtualSize,
        'COFF symbol value outside its section');
    end;
    Inc(Index);
  end;
end;

function BackedSection(const AParsed: TPEImage; const ARva, ASize: Cardinal): Integer;
var
  Index: Integer;
  Section: TPESection;
  Offset: UInt64;
begin
  for Index := 0 to High(AParsed.Sections) do
  begin
    Section := AParsed.Sections[Index];
    if ARva < Section.Rva then
      Continue;
    Offset := UInt64(ARva) - Section.Rva;
    if (Offset + ASize <= Section.VirtualSize) and
       (Offset + ASize <= Section.RawSize) then
      Exit(Index);
  end;
  Result := -1;
end;

function ParseImage(const AImage: TBytes; const AEdited: Boolean): TPEImage;
var
  Count, Characteristics, Subsystem: Word;
  Index, DirectoryIndex, SectionIndex: Integer;
  Offset, Rva, Size, NextRaw, NextRva, Span: Cardinal;
  Section: TPESection;
begin
  Require(UInt64(Length(AImage)) <= High(Cardinal), 'Image exceeds PE32 file size');
  RequireBytes(AImage, 0, DOS_HEADER_SIZE);
  Require(ReadWord(AImage, 0) = DOS_MAGIC, 'Missing DOS signature');
  Result.HeaderOffset := ReadCardinal(AImage, DOS_PE_OFFSET);
  Require((Result.HeaderOffset >= DOS_HEADER_SIZE) and
    (Result.HeaderOffset mod SizeOf(Cardinal) = 0), 'Unsupported PE header offset');
  RequireBytes(AImage, Result.HeaderOffset, COFF_HEADER_SIZE);
  Require(ReadCardinal(AImage, Result.HeaderOffset) = PE_MAGIC, 'Missing PE signature');
  Require(ReadWord(AImage, UInt64(Result.HeaderOffset) + COFF_MACHINE) = MACHINE_I386,
    'Only i386 images are supported');
  Count := ReadWord(AImage, UInt64(Result.HeaderOffset) + COFF_SECTION_COUNT);
  Require((Count > 0) and (Count <= MAX_SECTIONS), 'Unsupported section count');
  Characteristics := ReadWord(AImage, UInt64(Result.HeaderOffset) + COFF_CHARACTERISTICS);
  Require((Characteristics and (FILE_RELOCS_STRIPPED or FILE_EXECUTABLE_IMAGE or FILE_DLL)) =
    (FILE_RELOCS_STRIPPED or FILE_EXECUTABLE_IMAGE), 'Requires a fixed-base executable with relocations stripped');
  Require(ReadWord(AImage, UInt64(Result.HeaderOffset) + COFF_OPTIONAL_SIZE) = OPTIONAL_SIZE,
    'Unsupported optional header size');
  Result.OptionalOffset := CheckedCardinal(UInt64(Result.HeaderOffset) + COFF_HEADER_SIZE);
  RequireBytes(AImage, Result.OptionalOffset, OPTIONAL_SIZE);
  Require(ReadWord(AImage, Result.OptionalOffset) = PE32_MAGIC, 'Only PE32 is supported');
  Require(ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_SECTION_ALIGNMENT) = SECTION_ALIGNMENT,
    'Requires 4096-byte section alignment');
  Require(ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_FILE_ALIGNMENT) = FILE_ALIGNMENT,
    'Requires 512-byte file alignment');
  Require((ReadWord(AImage, Result.OptionalOffset + OPTIONAL_DLL_CHARACTERISTICS) and
    (not DLL_SUPPORTED_FLAGS and $FFFF)) = 0, 'Unsupported DLL characteristics (including ASLR or NO_SEH)');
  Subsystem := ReadWord(AImage, Result.OptionalOffset + OPTIONAL_SUBSYSTEM);
  Require((Subsystem = SUBSYSTEM_WINDOWS_GUI) or (Subsystem = SUBSYSTEM_WINDOWS_CUI),
    'Only Windows GUI or console executables are supported');
  Require(ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_WIN32_VERSION) = 0,
    'Nonzero Win32VersionValue');
  Require(ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_LOADER_FLAGS) = 0,
    'Nonzero loader flags');
  Require(ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_DIRECTORY_COUNT) = DIRECTORY_COUNT,
    'Requires the standard 16 data directories');
  Result.ImageBase := ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_IMAGE_BASE);
  Result.ImageSize := ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_IMAGE_SIZE);
  Result.HeadersSize := ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_HEADERS_SIZE);
  Require((Result.ImageBase <> 0) and (Result.ImageBase mod IMAGE_BASE_ALIGNMENT = 0),
    'Invalid fixed image base');
  CheckedCardinal(UInt64(Result.ImageBase) + Result.ImageSize);
  Require((Result.HeadersSize <> 0) and (Result.HeadersSize mod FILE_ALIGNMENT = 0),
    'Unaligned headers');
  RequireBytes(AImage, 0, Result.HeadersSize);
  Result.SectionsOffset := CheckedCardinal(UInt64(Result.OptionalOffset) + OPTIONAL_SIZE);
  Require(UInt64(Result.SectionsOffset) + UInt64(Count) * SECTION_SIZE <= Result.HeadersSize,
    'Section headers exceed SizeOfHeaders');
  ParseCOFFBounds(AImage, Result);
  NextRaw := Result.HeadersSize;
  NextRva := Aligned(Result.HeadersSize, SECTION_ALIGNMENT);
  SetLength(Result.Sections, Count);
  for Index := 0 to Count - 1 do
  begin
    Offset := Result.SectionsOffset + Cardinal(Index) * SECTION_SIZE;
    Section.HeaderOffset := Offset;
    ParseSectionName(AImage, Result, Section);
    Section.VirtualSize := ReadCardinal(AImage, Offset + SECTION_VIRTUAL_SIZE);
    Section.Rva := ReadCardinal(AImage, Offset + SECTION_RVA);
    Section.RawSize := ReadCardinal(AImage, Offset + SECTION_RAW_SIZE);
    Section.RawOffset := ReadCardinal(AImage, Offset + SECTION_RAW_OFFSET);
    Section.Characteristics := ReadCardinal(AImage, Offset + SECTION_CHARACTERISTICS);
    Require((Section.Characteristics and not Cardinal(SECTION_SUPPORTED_FLAGS)) = 0,
      'Unsupported section characteristics');
    Require((Section.VirtualSize > 0) and (Section.Rva = NextRva),
      'Empty, overlapping, unordered or noncontiguous virtual sections');
    Require(ReadCardinal(AImage, Offset + SECTION_RELOCATIONS) = 0, 'Section relocations unsupported');
    Require(ReadCardinal(AImage, Offset + SECTION_LINES) = 0, 'Section line numbers unsupported');
    Require(ReadWord(AImage, Offset + SECTION_RELOCATION_COUNT) = 0, 'Section relocations unsupported');
    Require(ReadWord(AImage, Offset + SECTION_LINE_COUNT) = 0, 'Section line numbers unsupported');
    if Section.RawSize = 0 then
      Require(Section.RawOffset = 0, 'Unsupported unbacked section offset')
    else
    begin
      Require((Section.RawSize mod FILE_ALIGNMENT = 0) and (Section.RawOffset = NextRaw),
        'Unaligned, overlapping, unordered or noncontiguous raw sections');
      RequireBytes(AImage, Section.RawOffset, Section.RawSize);
      NextRaw := CheckedCardinal(UInt64(NextRaw) + Section.RawSize);
    end;
    Span := Section.VirtualSize;
    if Section.RawSize > Span then
      Span := Section.RawSize;
    NextRva := Aligned(UInt64(Section.Rva) + Span, SECTION_ALIGNMENT);
    Require(not IsLoadConfig(AImage, Section) or (AEdited and (Index = Count - 1)),
      'Existing or misplaced .loadcfg section');
    Result.Sections[Index] := Section;
  end;
  Result.RawEnd := NextRaw;
  if Result.SymbolsOffset = 0 then
    Require(NextRaw = UInt64(Length(AImage)), 'Overlay or trailing data unsupported')
  else
    Require(NextRaw = Result.SymbolsOffset, 'COFF table must immediately follow section bytes');
  ValidateCOFFSymbols(AImage, Result, AEdited);
  Require(Result.ImageSize = NextRva, 'SizeOfImage does not match aligned sections');
  Rva := ReadCardinal(AImage, Result.OptionalOffset + OPTIONAL_ENTRY_POINT);
  SectionIndex := BackedSection(Result, Rva, 1);
  Require(SectionIndex >= 0, 'Entry point is not file-backed');
  Require((Result.Sections[SectionIndex].Characteristics and SECTION_EXECUTE) <> 0,
    'Entry point is not executable');
  for DirectoryIndex := 0 to DIRECTORY_COUNT - 1 do
  begin
    Offset := Result.OptionalOffset + OPTIONAL_DIRECTORIES + Cardinal(DirectoryIndex) * DIRECTORY_SIZE;
    Rva := ReadCardinal(AImage, Offset);
    Size := ReadCardinal(AImage, Offset + SizeOf(Cardinal));
    if (Rva = 0) and (Size = 0) then
      Continue;
    Require((Rva <> 0) and (Size <> 0), 'Incomplete data directory');
    Require((DirectoryIndex = DIRECTORY_IMPORT) or (DirectoryIndex = DIRECTORY_RESOURCE) or
      (DirectoryIndex = DIRECTORY_TLS) or (DirectoryIndex = DIRECTORY_IAT) or
      (AEdited and (DirectoryIndex = DIRECTORY_LOAD_CONFIG)), 'Unsupported nonzero data directory');
    SectionIndex := BackedSection(Result, Rva, Size);
    Require(SectionIndex >= 0, 'Directory is not contained in file-backed section bytes');
    if AEdited and (DirectoryIndex <> DIRECTORY_LOAD_CONFIG) then
      Require(SectionIndex <> Count - 1, 'Original directory points into .loadcfg');
  end;
end;

// Heap sort bounds work to O(n log n), even for adversarial input order.
procedure SiftDown(var AValues: TSERvaArray; const ARoot, ACount: NativeInt);
var
  Root, Child: NativeInt;
  Value: Cardinal;
begin
  Root := ARoot;
  while Root < ACount div 2 do
  begin
    Child := Root * 2 + 1;
    if (Child + 1 < ACount) and (AValues[Child] < AValues[Child + 1]) then
      Inc(Child);
    if AValues[Root] >= AValues[Child] then
      Exit;
    Value := AValues[Root];
    AValues[Root] := AValues[Child];
    AValues[Child] := Value;
    Root := Child;
  end;
end;

function SortedHandlers(const AParsed: TPEImage; const AHandlers: TSERvaArray): TSERvaArray;
var
  Index: NativeInt;
  SectionIndex: Integer;
  Value: Cardinal;
begin
  Require(Length(AHandlers) > 0, 'The complete handler list must not be empty');
  Require(UInt64(Length(AHandlers)) <= (High(Cardinal) - LOAD_CONFIG_SIZE) div HANDLER_SIZE,
    'Handler table size overflow');
  Result := Copy(AHandlers, 0, Length(AHandlers));
  for Index := Length(Result) div 2 - 1 downto 0 do
    SiftDown(Result, Index, Length(Result));
  for Index := High(Result) downto 1 do
  begin
    Value := Result[0];
    Result[0] := Result[Index];
    Result[Index] := Value;
    SiftDown(Result, 0, Index);
  end;
  for Index := 0 to High(Result) do
  begin
    if Index > 0 then
      Require(Result[Index] <> Result[Index - 1], 'Duplicate handler RVA');
    SectionIndex := BackedSection(AParsed, Result[Index], 1);
    Require(SectionIndex >= 0, 'Handler RVA is not file-backed executable code');
    Require((AParsed.Sections[SectionIndex].Characteristics and
      (SECTION_EXECUTE or SECTION_DISCARDABLE)) = SECTION_EXECUTE,
      'Handler RVA is not persistent executable code');
  end;
end;

function AddSafeSEH(const AImage: TBytes; const AHandlers: TSERvaArray): TBytes;
var
  Parsed: TPEImage;
  Handlers: TSERvaArray;
  HeaderOffset, RawOffset, PayloadSize, RawSize, NewImageSize, NewDataSize, TableAddress: Cardinal;
  NewFileSize: UInt64;
  Index: NativeInt;
begin
  Parsed := ParseImage(AImage, False);
  Handlers := SortedHandlers(Parsed, AHandlers);
  Require(Length(Parsed.Sections) < MAX_SECTIONS, 'No room for another section');
  HeaderOffset := Parsed.SectionsOffset + Cardinal(Length(Parsed.Sections)) * SECTION_SIZE;
  Require(UInt64(HeaderOffset) + SECTION_SIZE <= Parsed.HeadersSize, 'Insufficient section header room');
  RequireZero(AImage, HeaderOffset, SECTION_SIZE);
  PayloadSize := CheckedCardinal(LOAD_CONFIG_SIZE + UInt64(Length(Handlers)) * HANDLER_SIZE);
  RawSize := Aligned(PayloadSize, FILE_ALIGNMENT);
  NewImageSize := Aligned(UInt64(Parsed.ImageSize) + RawSize, SECTION_ALIGNMENT);
  CheckedCardinal(UInt64(Parsed.ImageBase) + NewImageSize);
  NewDataSize := CheckedCardinal(UInt64(ReadCardinal(AImage,
    Parsed.OptionalOffset + OPTIONAL_DATA_SIZE)) + RawSize);
  TableAddress := CheckedCardinal(UInt64(Parsed.ImageBase) + Parsed.ImageSize + LOAD_CONFIG_SIZE);
  RawOffset := Parsed.RawEnd;
  NewFileSize := UInt64(Length(AImage)) + RawSize;
  Require((NewFileSize <= High(Cardinal)) and (NewFileSize <= UInt64(High(NativeInt))),
    'Output file size overflow');
  // Copy before writing: dynamic arrays passed as const may still share storage.
  Result := Copy(AImage, 0, RawOffset);
  SetLength(Result, NativeInt(NewFileSize));
  if Parsed.SymbolsOffset <> 0 then
  begin
    Move(AImage[RawOffset], Result[RawOffset + RawSize], Length(AImage) - RawOffset);
    WriteCardinal(Result, Parsed.HeaderOffset + COFF_SYMBOLS, RawOffset + RawSize);
  end;
  for Index := 1 to SECTION_NAME_SIZE do
    Result[HeaderOffset + Cardinal(Index - 1)] := Ord(LOAD_CONFIG_NAME[Index]);
  WriteCardinal(Result, HeaderOffset + SECTION_VIRTUAL_SIZE, PayloadSize);
  WriteCardinal(Result, HeaderOffset + SECTION_RVA, Parsed.ImageSize);
  WriteCardinal(Result, HeaderOffset + SECTION_RAW_SIZE, RawSize);
  WriteCardinal(Result, HeaderOffset + SECTION_RAW_OFFSET, RawOffset);
  WriteCardinal(Result, HeaderOffset + SECTION_CHARACTERISTICS, SECTION_READONLY_DATA);
  WriteWord(Result, Parsed.HeaderOffset + COFF_SECTION_COUNT, Length(Parsed.Sections) + 1);
  WriteCardinal(Result, Parsed.OptionalOffset + OPTIONAL_DATA_SIZE, NewDataSize);
  WriteCardinal(Result, Parsed.OptionalOffset + OPTIONAL_IMAGE_SIZE, NewImageSize);
  WriteCardinal(Result, Parsed.OptionalOffset + OPTIONAL_CHECKSUM, 0);
  WriteCardinal(Result, Parsed.OptionalOffset + OPTIONAL_DIRECTORIES + DIRECTORY_LOAD_CONFIG * DIRECTORY_SIZE,
    Parsed.ImageSize);
  WriteCardinal(Result, Parsed.OptionalOffset + OPTIONAL_DIRECTORIES + DIRECTORY_LOAD_CONFIG * DIRECTORY_SIZE + SizeOf(Cardinal),
    LOAD_CONFIG_SIZE);
  // Microsoft /SAFESEH's 72-byte IMAGE_LOAD_CONFIG_DIRECTORY32 prefix:
  // SEHandlerTable is a VA; its entries are sorted image-relative addresses.
  // https://learn.microsoft.com/en-us/cpp/build/reference/safeseh-image-has-safe-exception-handlers
  WriteCardinal(Result, RawOffset, LOAD_CONFIG_SIZE);
  WriteCardinal(Result, RawOffset + LOAD_CONFIG_TABLE, TableAddress);
  WriteCardinal(Result, RawOffset + LOAD_CONFIG_COUNT, Length(Handlers));
  for Index := 0 to High(Handlers) do
    WriteCardinal(Result, RawOffset + LOAD_CONFIG_SIZE + Cardinal(Index) * HANDLER_SIZE, Handlers[Index]);
  ValidateSafeSEH(Result, AHandlers);
end;

procedure ValidateSafeSEH(const AImage: TBytes; const AHandlers: TSERvaArray);
var
  Parsed: TPEImage;
  Handlers: TSERvaArray;
  Section: TPESection;
  DirectoryOffset, PayloadSize, TableAddress: Cardinal;
  Index: NativeInt;
begin
  Parsed := ParseImage(AImage, True);
  Handlers := SortedHandlers(Parsed, AHandlers);
  Require(Length(Parsed.Sections) > 1, 'Missing appended load configuration');
  Section := Parsed.Sections[High(Parsed.Sections)];
  Require(IsLoadConfig(AImage, Section), 'Missing .loadcfg section');
  Require(Section.Characteristics = SECTION_READONLY_DATA, 'Load configuration must be read-only data');
  DirectoryOffset := Parsed.OptionalOffset + OPTIONAL_DIRECTORIES + DIRECTORY_LOAD_CONFIG * DIRECTORY_SIZE;
  Require((ReadCardinal(AImage, DirectoryOffset) = Section.Rva) and
    (ReadCardinal(AImage, DirectoryOffset + SizeOf(Cardinal)) = LOAD_CONFIG_SIZE),
    'Incorrect load configuration directory');
  PayloadSize := CheckedCardinal(LOAD_CONFIG_SIZE + UInt64(Length(Handlers)) * HANDLER_SIZE);
  Require((Section.VirtualSize = PayloadSize) and (Section.RawSize = Aligned(PayloadSize, FILE_ALIGNMENT)),
    'Incorrect load configuration section size');
  Require(ReadCardinal(AImage, Section.RawOffset) = LOAD_CONFIG_SIZE, 'Incorrect load configuration size');
  RequireZero(AImage, Section.RawOffset + SizeOf(Cardinal), LOAD_CONFIG_TABLE - SizeOf(Cardinal));
  TableAddress := CheckedCardinal(UInt64(Parsed.ImageBase) + Section.Rva + LOAD_CONFIG_SIZE);
  Require(ReadCardinal(AImage, Section.RawOffset + LOAD_CONFIG_TABLE) = TableAddress,
    'Incorrect handler table VA');
  Require(ReadCardinal(AImage, Section.RawOffset + LOAD_CONFIG_COUNT) = UInt64(Length(Handlers)),
    'Incorrect handler count');
  for Index := 0 to High(Handlers) do
    Require(ReadCardinal(AImage, Section.RawOffset + LOAD_CONFIG_SIZE + Cardinal(Index) * HANDLER_SIZE) = Handlers[Index],
      'Handler table does not exactly match the supplied complete set');
  RequireZero(AImage, Section.RawOffset + PayloadSize, Section.RawSize - PayloadSize);
  Require(ReadCardinal(AImage, Parsed.OptionalOffset + OPTIONAL_CHECKSUM) = 0, 'Expected cleared checksum');
  Require(ReadCardinal(AImage, Parsed.OptionalOffset + OPTIONAL_DATA_SIZE) >= Section.RawSize,
    'Initialized data size excludes load configuration');
end;

end.
