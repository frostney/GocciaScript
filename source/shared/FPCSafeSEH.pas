unit FPCSafeSEH;

{$I Shared.inc}

interface

uses
  SysUtils,

  PESafeSEH;

// Both operations verify the compiler, the exact linked toolchain objects,
// and the live map before accepting a table. No object is rewritten.
// Finalization requires the caller's compiler-owned -FU directory. Unpinned
// .o/.or inputs must live there and match a unique repository .pas/.dpr source.
// It records <map>.safeseh-inputs for Check after project scratch cleanup;
// keep the map/receipt pair, sources, and pinned toolchain objects available.
procedure FinalizeFPCExecutable(const AExecutable, AMapFile: string;
  const AObjectDirectory: string = '');
procedure CheckFPCExecutable(const AExecutable, AMapFile: string);

// Explicit inputs for audit tooling and tests. ACompilerVersion must be the
// result of querying the compiler, not a value copied from the manifest.
function ResolveFPCSafeSEH(const AImage: TBytes;
  const AMapFile, AManifestFile, ACompilerVersion: string;
  const AObjectDirectory: string = ''): TSERvaArray;

implementation

uses
  Classes,
  Process,

  FileUtils,
  SHA256;

const
  MANIFEST_NAME = 'config/win32-safeseh/fpc-3.2.4.manifest';
  MANIFEST_HEADER = 'FPC-SAFESEH 1';
  MAP_HEADER = 'Memory map (ImageBase=';
  READ_OBJECT = 'READOBJECT ';
  REMOVE_SECTION = 'Removing ';
  TEXT_PREFIX = '.text.n_';
  DOS_PE_OFFSET = 60;
  PE_OPTIONAL_OFFSET = 24;
  PE_IMAGE_BASE_OFFSET = 28;
  HASH_BUFFER_SIZE = 65536;

type
  TManifestObject = record
    Name: string;
    BaseName: string;
    Digest: string;
    Path: string;
    Required: Boolean;
  end;
  TManifestObjects = array of TManifestObject;
  TManifestSymbol = record
    ObjectIndex: Integer;
    Section: string;
    IsHandler: Boolean;
    Live: Boolean;
    Removed: Boolean;
    Address: Cardinal;
  end;
  TManifestSymbols = array of TManifestSymbol;
  TSourceSymbol = record
    Source: string;
    Digest: string;
    Section: string;
    Verified: Boolean;
  end;
  TSourceSymbols = array of TSourceSymbol;
  TManifest = record
    Version: string;
    Objects: TManifestObjects;
    Symbols: TManifestSymbols;
    SourceSymbols: TSourceSymbols;
  end;

procedure Require(const ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise ESafeSEH.Create(UTF8Encode(AMessage + '. Re-audit the Win32 toolchain before updating its manifest'));
end;

function ReadBytes(const AFileName: string): TBytes;
var
  Stream: TFileStream;
begin
  Result := nil;
  Stream := TFileStream.Create(UTF8Encode(AFileName), fmOpenRead or fmShareDenyWrite);
  try
    Require(Stream.Size <= High(Integer), 'Input file too large: ' + AFileName);
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

function ReadCardinal(const ABytes: TBytes; const AOffset: UInt64): Cardinal;
begin
  Require((AOffset <= UInt64(Length(ABytes))) and
    (UInt64(Length(ABytes)) - AOffset >= SizeOf(Cardinal)), 'Truncated PE header');
  Result := Cardinal(ABytes[AOffset]) or (Cardinal(ABytes[AOffset + 1]) shl 8) or
    (Cardinal(ABytes[AOffset + 2]) shl 16) or (Cardinal(ABytes[AOffset + 3]) shl 24);
end;

function HashFile(const AFileName: string): string;
var
  Stream: TFileStream;
  Context: TSHA256Context;
  Buffer: array[0..HASH_BUFFER_SIZE - 1] of Byte;
  Count: Integer;
begin
  Stream := TFileStream.Create(UTF8Encode(AFileName), fmOpenRead or fmShareDenyWrite);
  try
    SHA256Init(Context);
    repeat
      Count := Stream.Read(Buffer, SizeOf(Buffer));
      if Count > 0 then
        SHA256Update(Context, @Buffer[0], Count);
    until Count = 0;
    Result := SHA256FinalHex(Context);
  finally
    Stream.Free;
  end;
end;

function Token(var ALine: string): string;
var
  Position: Integer;
begin
  ALine := TrimLeft(ALine);
  Position := 1;
  while (Position <= Length(ALine)) and (ALine[Position] > ' ') do
    Inc(Position);
  Result := Copy(ALine, 1, Position - 1);
  Delete(ALine, 1, Position - 1);
  ALine := TrimLeft(ALine);
end;

function NormalPath(const APath: string): string;
begin
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
end;

function ObjectName(const APath: string): string;
var
  Path: string;
begin
  Path := NormalPath(APath);
  Result := LowerCase(Copy(Path, LastDelimiter('/', Path) + 1, MaxInt));
end;

function FindObject(const AManifest: TManifest; const AName: string): Integer;
var
  Index: Integer;
  Name: string;
begin
  Name := ObjectName(AName);
  for Index := 0 to High(AManifest.Objects) do
    if AManifest.Objects[Index].BaseName = Name then
      Exit(Index);
  Result := -1;
end;

function FindSymbol(const AManifest: TManifest; const AObjectIndex: Integer;
  const ASection: string): Integer;
var
  Index: Integer;
begin
  for Index := 0 to High(AManifest.Symbols) do
    if (AManifest.Symbols[Index].ObjectIndex = AObjectIndex) and
      (AManifest.Symbols[Index].Section = LowerCase(ASection)) then
      Exit(Index);
  Result := -1;
end;

function IsSEHCandidate(const ASection: string): Boolean;
var
  Name: string;
  Position: Integer;
begin
  Name := LowerCase(ASection);
  if Copy(Name, 1, Length(TEXT_PREFIX)) <> TEXT_PREFIX then
    Exit(False);
  Delete(Name, 1, Length(TEXT_PREFIX));
  // FPC uses P$ for programs and $ components for static methods/nested
  // routines. None of those prefixes exempts a handler-like identifier.
  Position := Pos('_$$_', Name);
  while Position > 0 do
  begin
    Delete(Name, 1, Position + Length('_$$_') - 1);
    Position := Pos('_$$_', Name);
  end;
  Position := Pos('$', Name);
  if Position > 0 then
    SetLength(Name, Position - 1);
  Result := (Pos('handler', Name) > 0) or (Pos('except_safecall', Name) > 0);
end;

procedure ReadManifest(const AFileName: string; out AManifest: TManifest);
var
  Lines: TStringList;
  Index, ObjectIndex, SymbolIndex, Digit, Previous: Integer;
  Line, Kind, Name, Value: string;
  HasTarget: Boolean;
begin
  AManifest := Default(TManifest);
  HasTarget := False;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(UTF8Encode(AFileName));
    Require((Lines.Count > 0) and (string(Lines[0]) = MANIFEST_HEADER), 'Unsupported SafeSEH manifest');
    for Index := 1 to Lines.Count - 1 do
    begin
      Line := Trim(string(Lines[Index]));
      if (Line = '') or (Line[1] = '#') then
        Continue;
      Kind := Token(Line);
      Name := Token(Line);
      if Kind = 'version' then
      begin
        Require((AManifest.Version = '') and (Name <> '') and (Line = ''), 'Invalid manifest version');
        AManifest.Version := Name;
      end else if Kind = 'target' then
      begin
        Require(not HasTarget and (Name = 'i386-win32') and (Line = ''), 'Invalid manifest target');
        HasTarget := True;
      end else if (Kind = 'object') or (Kind = 'required-object') then
      begin
        Value := Token(Line);
        Require((Name <> '') and (FindObject(AManifest, Name) < 0), 'Duplicate or empty manifest object: ' + Name);
        Require((Length(Value) = 64) and (Line = ''), 'Invalid manifest SHA-256: ' + Name);
        for Digit := 1 to Length(Value) do
          Require(CharInSet(Value[Digit], ['0'..'9', 'a'..'f']), 'Invalid manifest SHA-256: ' + Name);
        ObjectIndex := Length(AManifest.Objects);
        SetLength(AManifest.Objects, ObjectIndex + 1);
        AManifest.Objects[ObjectIndex].Name := Name;
        AManifest.Objects[ObjectIndex].BaseName := ObjectName(Name);
        AManifest.Objects[ObjectIndex].Digest := Value;
        AManifest.Objects[ObjectIndex].Required := Kind = 'required-object';
      end else if Kind = 'source-nonseh' then
      begin
        Value := Token(Line);
        Require((Name <> '') and (Length(Value) = 64), 'Invalid source-bound non-SEH entry');
        for Digit := 1 to Length(Value) do
          Require(CharInSet(Value[Digit], ['0'..'9', 'a'..'f']), 'Invalid source-bound SHA-256');
        SymbolIndex := Length(AManifest.SourceSymbols);
        SetLength(AManifest.SourceSymbols, SymbolIndex + 1);
        AManifest.SourceSymbols[SymbolIndex].Source :=
          ExpandFileName(ExtractFilePath(ExpandFileName(AFileName)) + Name);
        AManifest.SourceSymbols[SymbolIndex].Digest := Value;
        AManifest.SourceSymbols[SymbolIndex].Section := LowerCase(Token(Line));
        Require(IsSEHCandidate(AManifest.SourceSymbols[SymbolIndex].Section) and
          (Line = ''), 'Invalid source-bound symbol');
        for Previous := 0 to SymbolIndex - 1 do
          Require(AManifest.SourceSymbols[Previous].Section <>
            AManifest.SourceSymbols[SymbolIndex].Section, 'Duplicate source-bound symbol');
      end else if (Kind = 'handler') or (Kind = 'nonseh') then
      begin
        Value := LowerCase(Token(Line));
        ObjectIndex := FindObject(AManifest, Name);
        Require((ObjectIndex >= 0) and (Value <> '') and (Line = ''), 'Invalid manifest symbol: ' + Name);
        Require(Copy(Value, 1, Length(TEXT_PREFIX)) = TEXT_PREFIX, 'Manifest symbol must name a text section');
        Require(FindSymbol(AManifest, ObjectIndex, Value) < 0, 'Duplicate manifest symbol: ' + Value);
        SymbolIndex := Length(AManifest.Symbols);
        SetLength(AManifest.Symbols, SymbolIndex + 1);
        AManifest.Symbols[SymbolIndex].ObjectIndex := ObjectIndex;
        AManifest.Symbols[SymbolIndex].Section := Value;
        AManifest.Symbols[SymbolIndex].IsHandler := Kind = 'handler';
      end else
        Require(False, 'Unknown manifest directive: ' + Kind);
    end;
    Require((AManifest.Version <> '') and HasTarget and
      (Length(AManifest.Objects) > 0) and (Length(AManifest.Symbols) > 0), 'Incomplete SafeSEH manifest');
  finally
    Lines.Free;
  end;
end;

function HexCardinal(const AValue: string): Cardinal;
var
  Value: QWord;
begin
  Require((Copy(AValue, 1, 2) = '0x') and
    TryStrToQWord('$' + Copy(AValue, 3, MaxInt), Value) and
    (Value <= High(Cardinal)), 'Invalid map address: ' + AValue);
  Result := Cardinal(Value);
end;

function ReviewedSourceSymbol(var AManifest: TManifest;
  const ASection, AOwner: string): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(AManifest.SourceSymbols) do
    if (AManifest.SourceSymbols[Index].Section = LowerCase(ASection)) and
      (ObjectName(ChangeFileExt(AManifest.SourceSymbols[Index].Source, '.o')) = ObjectName(AOwner)) then
    begin
      if not AManifest.SourceSymbols[Index].Verified then
      begin
        Require(FileExists(AManifest.SourceSymbols[Index].Source),
          'Reviewed non-SEH source unavailable: ' + AManifest.SourceSymbols[Index].Source);
        Require(HashFile(AManifest.SourceSymbols[Index].Source) = AManifest.SourceSymbols[Index].Digest,
          'Reviewed non-SEH source SHA-256 mismatch: ' + AManifest.SourceSymbols[Index].Source);
        AManifest.SourceSymbols[Index].Verified := True;
      end;
      Exit(True);
    end;
  Result := False;
end;

function ProjectRoot(const AManifestFile: string): string;
begin
  Result := ExpandFileName(ExtractFilePath(ExpandFileName(AManifestFile)) + '..' + PathDelim + '..');
end;

procedure CollectProjectSources(const ARoot: string; const AFiles: TStrings);
const
  SOURCE_DIRECTORIES: array[0..1] of string = ('source', 'souffle');
var
  Files: TStringList;
  Directory: string;
  Index, FileIndex: Integer;
begin
  for Index := Low(SOURCE_DIRECTORIES) to High(SOURCE_DIRECTORIES) do
  begin
    Directory := IncludeTrailingPathDelimiter(ARoot) + SOURCE_DIRECTORIES[Index];
    Files := FindAllFiles(Directory, ['.pas', '.dpr']);
    try
      for FileIndex := 0 to Files.Count - 1 do
        AFiles.Add(Files[FileIndex]);
    finally
      Files.Free;
    end;
  end;
  // The host tool is the only root program used by this workflow.
  if FileExists(IncludeTrailingPathDelimiter(ARoot) + 'safeseh.pas') then
    AFiles.Add(IncludeTrailingPathDelimiter(ARoot) + 'safeseh.pas');
end;

function ProjectSource(const AObjectPath: string; const AFiles: TStrings): string;
var
  Index: Integer;
  Name, Extension: string;
begin
  Result := '';
  Extension := LowerCase(ExtractFileExt(AObjectPath));
  Require((Extension = '.o') or (Extension = '.or'), 'Unreviewed linked input: ' + AObjectPath);
  Name := ObjectName(ChangeFileExt(AObjectPath, '.o'));
  for Index := 0 to AFiles.Count - 1 do
    if ObjectName(ChangeFileExt(string(AFiles[Index]), '.o')) = Name then
    begin
      Require(Result = '', 'Ambiguous project source for input: ' + AObjectPath);
      Result := string(AFiles[Index]);
    end;
  Require(Result <> '', 'Unreviewed linked input without project source: ' + AObjectPath);
end;

procedure RequireReceiptField(const AValue: string);
begin
  Require((Pos(#9, AValue) = 0) and (Pos(#10, AValue) = 0) and
    (Pos(#13, AValue) = 0), 'Unsupported control character in input provenance');
end;

procedure LoadReceipt(const AMapFile: string; const AImage: TBytes;
  const AVersion: string; const ALines: TStrings);
begin
  Require(FileExists(AMapFile + '.safeseh-inputs'),
    'Missing build-owned input receipt: ' + AMapFile + '.safeseh-inputs');
  ALines.LoadFromFile(UTF8Encode(AMapFile + '.safeseh-inputs'));
  Require((ALines.Count >= 6) and (ALines[0] = 'FPC-SAFESEH-INPUTS 1'), 'Invalid input receipt');
  Require(string(ALines[1]) = 'map ' + HashFile(AMapFile), 'Input receipt map SHA-256 mismatch');
  Require(string(ALines[2]) = 'image ' + SHA256Hex(AImage), 'Input receipt image SHA-256 mismatch');
  Require(string(ALines[3]) = 'version ' + AVersion, 'Input receipt compiler version mismatch');
  Require((Copy(string(ALines[4]), 1, 4) = 'cwd ') and
    (Copy(string(ALines[5]), 1, 10) = 'directory '), 'Invalid input receipt directories');
end;

procedure VerifyProjectInput(const APath, AObjectDirectory, ARoot: string;
  const ASources, AReceipt, AProjectLines: TStrings);
var
  Source, ObjectPath, Directory, SourceDigest, ObjectDigest, Row: string;
  Fields: TStringList;
  Index, Matches: Integer;
begin
  Source := ProjectSource(APath, ASources);
  RequireReceiptField(APath);
  RequireReceiptField(Source);
  Require(HostPathIsRegularFile(Source), 'Project source is not a regular file: ' + Source);
  Require(Copy(CanonicalHostPath(Source), 1, Length(IncludeTrailingPathDelimiter(CanonicalHostPath(ARoot)))) =
    IncludeTrailingPathDelimiter(CanonicalHostPath(ARoot)), 'Project source is outside the repository: ' + Source);
  SourceDigest := HashFile(Source);
  if AObjectDirectory <> '' then
  begin
    Directory := CanonicalHostPath(AObjectDirectory);
    Require(Directory <> '', 'Build-owned object directory unavailable: ' + AObjectDirectory);
    Require(HostPathIsRegularFile(APath), 'Project input is not a regular file: ' + APath);
    ObjectPath := CanonicalHostPath(APath);
    Require(ExtractFileDir(ObjectPath) = ExcludeTrailingPathDelimiter(Directory),
      'Project input is outside the build-owned object directory: ' + APath);
    ObjectDigest := HashFile(ObjectPath);
    RequireReceiptField(ObjectPath);
    Row := APath + #9 + Source + #9 + SourceDigest + #9 + ObjectDigest + #9 + ObjectPath;
    AProjectLines.Add(UTF8Encode(Row));
    Exit;
  end;
  Require(AReceipt.Count >= 6, 'Missing build-owned input provenance: ' + APath);
  Fields := TStringList.Create;
  try
    Fields.Delimiter := #9;
    Fields.StrictDelimiter := True;
    Fields.QuoteChar := #0;
    Matches := 0;
    for Index := 6 to AReceipt.Count - 1 do
    begin
      Fields.DelimitedText := AReceipt[Index];
      Require(Fields.Count = 5, 'Malformed project input receipt row');
      if UTF8Decode(Fields[0]) <> APath then
        Continue;
      Inc(Matches);
      Require(UTF8Decode(Fields[1]) = Source, 'Project source differs from input receipt: ' + APath);
      Require(string(Fields[2]) = SourceDigest, 'Project source SHA-256 differs from input receipt: ' + Source);
      // This is the physical path captured while the compiled object existed.
      // Its lexical spelling remains checkable after scratch cleanup.
      ObjectPath := UTF8Decode(Fields[4]);
      Require(IsAbsoluteHostPath(ObjectPath) and (ExpandFileName(ObjectPath) = ObjectPath),
        'Invalid absolute input path in receipt');
      Directory := Copy(UTF8Decode(AReceipt[5]), 11, MaxInt);
      Require(ExtractFileDir(ObjectPath) = Directory,
        'Receipt input is outside its build-owned directory: ' + APath);
      if FileExists(ObjectPath) or HostPathIsSymlink(ObjectPath) then
      begin
        Require(HostPathIsRegularFile(ObjectPath), 'Receipt object is not a regular file: ' + APath);
        Require(HashFile(ObjectPath) = string(Fields[3]), 'Project object SHA-256 differs from input receipt: ' + APath);
      end;
      AProjectLines.Add(AReceipt[Index]);
    end;
    Require(Matches = 1, 'Missing or duplicate project input receipt: ' + APath);
  finally
    Fields.Free;
  end;
end;

procedure ReadMap(const AMapFile: string; var AManifest: TManifest;
  const AImageBase: Cardinal; const AObjectDirectory, ARoot: string;
  const ASources, AReceipt, AProjectLines: TStrings);
var
  Map: TextFile;
  RawLine: AnsiString;
  Line, Name, Path, Section, AddressText, SizeText, Owner: string;
  LiveSection, LiveOwner: string;
  ObjectIndex, SymbolIndex, Position, HeaderCount: Integer;
  Address, SectionSize, LiveAddress: Cardinal;
  InMap: Boolean;
begin
  HeaderCount := 0;
  InMap := False;
  Section := '';
  LiveSection := '';
  LiveOwner := '';
  LiveAddress := 0;
  AssignFile(Map, UTF8Encode(AMapFile));
  Reset(Map);
  try
    while not Eof(Map) do
    begin
      ReadLn(Map, RawLine);
      Line := Trim(UTF8Decode(RawLine));
      if Copy(Line, 1, Length(READ_OBJECT)) = READ_OBJECT then
      begin
        Require(not InMap, 'Input declaration after memory map');
        Path := Trim(Copy(Line, Length(READ_OBJECT) + 1, MaxInt));
        ObjectIndex := FindObject(AManifest, Path);
        if ObjectIndex >= 0 then
        begin
          Require(AManifest.Objects[ObjectIndex].Path = '', 'Duplicate linked toolchain object: ' + Path);
          AManifest.Objects[ObjectIndex].Path := Path;
        end else
          VerifyProjectInput(Path, AObjectDirectory, ARoot, ASources, AReceipt, AProjectLines);
        Continue;
      end;
      if Copy(Line, 1, Length(REMOVE_SECTION)) = REMOVE_SECTION then
      begin
        Require(not InMap, 'Discarded section after memory map');
        Delete(Line, 1, Length(REMOVE_SECTION));
        Position := Pos('(', Line);
        if (Position > 0) and (Line[Length(Line)] = ')') then
        begin
          Name := Copy(Line, 1, Position - 1);
          ObjectIndex := FindObject(AManifest, Name);
          Name := Copy(Line, Position + 1, Length(Line) - Position - 1);
          SymbolIndex := FindSymbol(AManifest, ObjectIndex, Name);
          if SymbolIndex >= 0 then
          begin
            Require(not AManifest.Symbols[SymbolIndex].Removed, 'Duplicate discarded handler section: ' + Name);
            AManifest.Symbols[SymbolIndex].Removed := True;
          end;
        end;
        Continue;
      end;
      if Copy(Line, 1, Length(MAP_HEADER)) = MAP_HEADER then
      begin
        Inc(HeaderCount);
        Require((HeaderCount = 1) and (Line[Length(Line)] = ')'), 'Invalid memory map header');
        Name := Copy(Line, Length(MAP_HEADER) + 1, Length(Line) - Length(MAP_HEADER) - 1);
        Require(HexCardinal(Name) = AImageBase, 'Map image base differs from executable');
        InMap := True;
        Continue;
      end;
      if not InMap or (Line = '') then
        Continue;
      Name := Token(Line);
      if (Name <> '') and (Name[1] = '.') then
      begin
        Section := Name;
        if Line = '' then
          Continue;
        Name := Token(Line);
      end else if Section = '' then
      begin
        // Public aliases can name an OS handler even when its input section
        // has an unrelated name. A reviewed alias must denote the same entry
        // address of a reviewed section in the same object. Data aliases
        // (VMT/RTTI/variables) are not routine entry points.
        AddressText := Name;
        Name := Token(Line);
        if (Copy(AddressText, 1, 2) = '0x') and
          ((LiveSection = '.text') or (Copy(LiveSection, 1, 6) = '.text.')) and
          IsSEHCandidate(TEXT_PREFIX + Name) then
        begin
          ObjectIndex := FindObject(AManifest, LiveOwner);
          Require((HexCardinal(AddressText) = LiveAddress) and
            ((FindSymbol(AManifest, ObjectIndex, LiveSection) >= 0) or
            ReviewedSourceSymbol(AManifest, LiveSection, LiveOwner)),
            'Unreviewed handler-like map symbol: ' + Name);
        end;
        Continue;
      end;
      // The next line after a long section name carries address/size/owner.
      // Symbol aliases carry address/name only and are not section records.
      AddressText := Name;
      SizeText := Token(Line);
      Owner := Trim(Line);
      if (Copy(AddressText, 1, 2) <> '0x') or
        (Copy(SizeText, 1, 2) <> '0x') or (Owner = '') then
      begin
        Require(not IsSEHCandidate(Section), 'Malformed handler section in map: ' + Section);
        Section := '';
        Continue;
      end;
      LiveSection := Section;
      LiveOwner := Owner;
      LiveAddress := HexCardinal(AddressText);
      ObjectIndex := FindObject(AManifest, Owner);
      SymbolIndex := FindSymbol(AManifest, ObjectIndex, Section);
      if SymbolIndex >= 0 then
      begin
        Address := HexCardinal(AddressText);
        SectionSize := HexCardinal(SizeText);
        Require(not AManifest.Symbols[SymbolIndex].Live, 'Duplicate live handler section: ' + Section);
        Require(not AManifest.Symbols[SymbolIndex].Removed, 'Handler is both live and discarded: ' + Section);
        Require((Address >= AImageBase) and (SectionSize > 0), 'Invalid live handler section: ' + Section);
        AManifest.Symbols[SymbolIndex].Live := True;
        AManifest.Symbols[SymbolIndex].Address := Address;
      end else
        Require(not IsSEHCandidate(Section) or ReviewedSourceSymbol(AManifest, Section, Owner),
          'Unreviewed handler-like map section: ' + Section + ' in ' + Owner);
      Section := '';
    end;
    Require(HeaderCount = 1, 'Missing memory map image base');
    Require(Section = '', 'Truncated memory map section');
  finally
    CloseFile(Map);
  end;
end;

function ResolveInputs(const AImage: TBytes;
  const AMapFile, AManifestFile, ACompilerVersion, AObjectDirectory: string;
  const AProjectLines: TStrings): TSERvaArray;
var
  Manifest: TManifest;
  ImageBase: Cardinal;
  Index, ObjectIndex: Integer;
  Path: string;
  Sources, Receipt: TStringList;
begin
  Result := nil;
  ReadManifest(AManifestFile, Manifest);
  Require(Trim(ACompilerVersion) = Manifest.Version, 'FPC version differs from reviewed manifest: ' + ACompilerVersion);
  ImageBase := ReadCardinal(AImage, UInt64(ReadCardinal(AImage, DOS_PE_OFFSET)) +
    PE_OPTIONAL_OFFSET + PE_IMAGE_BASE_OFFSET);
  Sources := TStringList.Create;
  Receipt := TStringList.Create;
  try
    CollectProjectSources(ProjectRoot(AManifestFile), Sources);
    if (AObjectDirectory = '') and FileExists(AMapFile + '.safeseh-inputs') then
      LoadReceipt(AMapFile, AImage, ACompilerVersion, Receipt);
    ReadMap(AMapFile, Manifest, ImageBase, AObjectDirectory,
      ProjectRoot(AManifestFile), Sources, Receipt, AProjectLines);
    if Receipt.Count > 0 then
      Require(AProjectLines.Count = Receipt.Count - 6, 'Unused or duplicate project input receipt rows');
  finally
    Receipt.Free;
    Sources.Free;
  end;
  for Index := 0 to High(Manifest.Objects) do
  begin
    Path := Manifest.Objects[Index].Path;
    Require(not Manifest.Objects[Index].Required or (Path <> ''),
      'Missing required toolchain object: ' + Manifest.Objects[Index].Name);
    if Path = '' then
      Continue;
    Require(FileExists(Path), 'Linked toolchain object unavailable: ' + Path);
    Require(HashFile(Path) = Manifest.Objects[Index].Digest, 'Toolchain object SHA-256 mismatch: ' + Path);
  end;
  for Index := 0 to High(Manifest.Symbols) do
  begin
    ObjectIndex := Manifest.Symbols[Index].ObjectIndex;
    if Manifest.Objects[ObjectIndex].Path = '' then
    begin
      Require(not Manifest.Symbols[Index].Live, 'Handler has no linked input object: ' + Manifest.Symbols[Index].Section);
      Continue;
    end;
    if not Manifest.Symbols[Index].IsHandler then
      Continue;
    Require(Manifest.Symbols[Index].Live or Manifest.Symbols[Index].Removed,
      'Missing manifest handler in map: ' + Manifest.Symbols[Index].Section);
    if Manifest.Symbols[Index].Live then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Manifest.Symbols[Index].Address - ImageBase;
    end;
  end;
  Require(Length(Result) > 0, 'No live SafeSEH handlers');
end;

function ResolveFPCSafeSEH(const AImage: TBytes;
  const AMapFile, AManifestFile, ACompilerVersion: string;
  const AObjectDirectory: string): TSERvaArray;
var
  ProjectLines: TStringList;
begin
  ProjectLines := TStringList.Create;
  try
    Result := ResolveInputs(AImage, AMapFile, AManifestFile, ACompilerVersion,
      AObjectDirectory, ProjectLines);
  finally
    ProjectLines.Free;
  end;
end;

function ManifestFileName: string;
var
  Directory, Parent: string;
begin
  Result := SysUtils.GetEnvironmentVariable('FPC_SAFESEH_MANIFEST');
  if Result <> '' then
  begin
    Require(FileExists(Result), 'SafeSEH manifest unavailable: ' + Result);
    Exit;
  end;
  // Compiled tools normally live under build/. Walk toward the repository;
  // instantfpc's cache is outside it, so also support the repository cwd.
  Directory := ExtractFileDir(ExpandFileName(ParamStr(0)));
  repeat
    Result := IncludeTrailingPathDelimiter(Directory) + MANIFEST_NAME;
    if FileExists(Result) then
      Exit;
    Parent := ExtractFileDir(Directory);
    if Parent = Directory then
      Break;
    Directory := Parent;
  until Directory = '';
  Result := ExpandFileName(MANIFEST_NAME);
  Require(FileExists(Result), 'Set FPC_SAFESEH_MANIFEST to the reviewed manifest path');
end;

function CompilerVersion: string;
var
  Compiler: string;
  Output: AnsiString;
begin
  Compiler := SysUtils.GetEnvironmentVariable('FPC_SAFESEH_COMPILER');
  if Compiler = '' then
    Compiler := 'fpc';
  Require(RunCommand(UTF8Encode(Compiler), ['-Pi386', '-Twin32', '-iW'], Output),
    'Cannot query the i386-win32 FPC compiler version');
  Result := Trim(UTF8Decode(Output));
end;

procedure CheckFPCExecutable(const AExecutable, AMapFile: string);
var
  Image: TBytes;
  Handlers: TSERvaArray;
begin
  Require(HostPathIsRegularFile(AExecutable), 'Executable is not a regular file: ' + AExecutable);
  Require(FileExists(AMapFile + '.safeseh-inputs'), 'Missing build-owned input receipt');
  Image := ReadBytes(AExecutable);
  Handlers := ResolveFPCSafeSEH(Image, AMapFile, ManifestFileName, CompilerVersion);
  ValidateSafeSEH(Image, Handlers);
end;

procedure FinalizeFPCExecutable(const AExecutable, AMapFile: string;
  const AObjectDirectory: string);
var
  Image, Output, ReceiptBytes: TBytes;
  Entries: TSERvaArray;
  Error, Version, Text: string;
  ProjectLines, Receipt: TStringList;
  Permissions: Cardinal;
  Encoded: UTF8String;
begin
  Require(HostPathIsRegularFile(AExecutable), 'Executable is not a regular file: ' + AExecutable);
  Image := ReadBytes(AExecutable);
  ProjectLines := TStringList.Create;
  Receipt := TStringList.Create;
  try
    Require(AObjectDirectory <> '', 'Finalization requires the build-owned compiler object directory');
    Version := CompilerVersion;
    Entries := ResolveInputs(Image, AMapFile, ManifestFileName, Version,
      AObjectDirectory, ProjectLines);
    Output := AddSafeSEH(Image, Entries); // Validates its returned bytes.
    Permissions := &666;
    {$IFDEF UNIX}
    Require(TryHostFileMode(AExecutable, Permissions), 'Cannot read executable permissions');
    {$ENDIF}
    // Refuse a pre-existing link before writing either build artifact.
    Require(not HostPathIsSymlink(AExecutable + '.safeseh.tmp'), 'SafeSEH temporary path is a symlink');
    Receipt.Add('FPC-SAFESEH-INPUTS 1');
    Receipt.Add(UTF8Encode('map ' + HashFile(AMapFile)));
    Receipt.Add(UTF8Encode('image ' + SHA256Hex(Output)));
    Receipt.Add(UTF8Encode('version ' + Version));
    RequireReceiptField(GetCurrentDir);
    RequireReceiptField(CanonicalHostPath(AObjectDirectory));
    Receipt.Add(UTF8Encode('cwd ' + GetCurrentDir));
    Receipt.Add(UTF8Encode('directory ' + CanonicalHostPath(AObjectDirectory)));
    Receipt.AddStrings(ProjectLines);
    Text := UTF8Decode(Receipt.Text);
    Encoded := UTF8Encode(Text);
    SetLength(ReceiptBytes, Length(Encoded));
    if Length(Encoded) > 0 then
      Move(Encoded[1], ReceiptBytes[0], Length(Encoded));
    if not ReplaceHostFile(AMapFile + '.safeseh-inputs',
      AMapFile + '.safeseh-inputs.tmp', ReceiptBytes, &666, True, False, Error) then
      Require(False, Error);
    // The shared helper compares a read-back from its exclusively held file
    // handle with Output before replacing the image; no second in-memory
    // traversal of AddSafeSEH's identical output is needed.
    if not ReplaceHostFile(AExecutable, AExecutable + '.safeseh.tmp',
      Output, Permissions, True, True, Error) then
      Require(False, Error);
  finally
    Receipt.Free;
    ProjectLines.Free;
  end;
end;

end.
