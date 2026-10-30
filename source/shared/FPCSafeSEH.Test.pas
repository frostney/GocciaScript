program FPCSafeSEH.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  FileUtils,
  FPCSafeSEH,
  PESafeSEH,
  SHA256,
  TestingPascalLibrary;

const
  FIXTURE_VERSION = '3.2.4-rc2+1';
  FIXTURE_BASE = $10000000;
  FIXTURE_RVA = $1000;
  FIXTURE_SECTION = '.text.n_system_$$_nullhandler$crc12345678';
  OPTIONAL_SECTION = '.text.n_signals_$$_signals_exception_handler$pointer';
  FIXTURE_HEADER = 'Memory map (ImageBase=0x10000000)';

type
  TFPCSafeSEHTests = class(TTestSuite)
  private
    FDirectory: string;
    FObjectFile: string;
    FOptionalFile: string;
    FManifestFile: string;
    FMapFile: string;
    FImage: TBytes;
    function Manifest: string;
    function Map: string;
    procedure WriteText(const AFileName, AText: string);
    function Resolve(const AManifest, AMap: string;
      const AVersion: string = FIXTURE_VERSION): TSERvaArray;
    procedure Reject(const AManifest, AMap, AReason: string;
      const AVersion: string = FIXTURE_VERSION);
    procedure TestValidMap;
    procedure TestMissingHandler;
    procedure TestUnknownHandler;
    procedure TestObjectMismatch;
    procedure TestMissingObject;
    procedure TestVersionMismatch;
    procedure TestBaseMismatch;
    procedure TestOptionalUnit;
    procedure TestDeadStripping;
    procedure TestDeadStrippingParenthesizedPath;
    procedure TestDuplicateEvidence;
    procedure TestMalformedManifest;
    procedure TestMalformedMap;
    procedure TestUnreviewedInput;
    procedure TestHandlerOwnership;
    procedure TestSourceBinding;
    procedure TestStagedProject;
    procedure TestProjectProvenance;
    procedure TestProgramAndMethodNames;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetupTests; override;
  end;

constructor TFPCSafeSEHTests.Create;
var
  Temporary: string;
begin
  inherited Create('FPCSafeSEH');
  Temporary := string(GetTempFileName('', 'seh'));
  DeleteFile(Temporary);
  FDirectory := Temporary;
  if not CreateDir(FDirectory) then
    raise Exception.Create('Cannot create SafeSEH test directory');
  FObjectFile := FDirectory + PathDelim + 'system.o';
  FOptionalFile := FDirectory + PathDelim + 'signals.o';
  ForceDirectories(FDirectory + PathDelim + 'config' + PathDelim + 'win32-safeseh');
  ForceDirectories(FDirectory + PathDelim + 'source' + PathDelim + 'shared');
  ForceDirectories(FDirectory + PathDelim + 'compiled');
  FManifestFile := FDirectory + PathDelim + 'config' + PathDelim + 'win32-safeseh' + PathDelim + 'fixture.manifest';
  FMapFile := FDirectory + PathDelim + 'fixture.map';
  WriteText(FObjectFile, 'reviewed object bytes');
  WriteText(FOptionalFile, 'reviewed object bytes');
  SetLength(FImage, 256);
  FImage[60] := $80;
  FImage[$80 + 24 + 28 + 3] := $10;
end;

destructor TFPCSafeSEHTests.Destroy;
begin
  DeleteFile(FObjectFile);
  DeleteFile(FOptionalFile);
  DeleteFile(FManifestFile);
  DeleteFile(FMapFile);
  RemoveDir(FDirectory + PathDelim + 'config' + PathDelim + 'win32-safeseh');
  RemoveDir(FDirectory + PathDelim + 'config');
  RemoveDir(FDirectory + PathDelim + 'source' + PathDelim + 'shared');
  RemoveDir(FDirectory + PathDelim + 'source');
  RemoveDir(FDirectory + PathDelim + 'compiled');
  RemoveDir(FDirectory);
  inherited Destroy;
end;

procedure TFPCSafeSEHTests.WriteText(const AFileName, AText: string);
var
  Stream: TFileStream;
  Bytes: UTF8String;
begin
  Bytes := UTF8Encode(AText);
  Stream := TFileStream.Create(UTF8Encode(AFileName), fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function TFPCSafeSEHTests.Manifest: string;
var
  Bytes: TBytes;
  Text: UTF8String;
begin
  Text := 'reviewed object bytes';
  SetLength(Bytes, Length(Text));
  Move(Text[1], Bytes[0], Length(Text));
  Result := 'FPC-SAFESEH 1' + LineEnding +
    'version ' + FIXTURE_VERSION + LineEnding +
    'target i386-win32' + LineEnding +
    'required-object rtl/system.o ' + SHA256Hex(Bytes) + LineEnding +
    'object rtl/signals.o ' + SHA256Hex(Bytes) + LineEnding +
    'handler rtl/system.o ' + FIXTURE_SECTION + LineEnding +
    'handler rtl/signals.o ' + OPTIONAL_SECTION + LineEnding;
end;

function TFPCSafeSEHTests.Map: string;
begin
  Result := 'Used linker script' + LineEnding +
    'READOBJECT ' + FObjectFile + LineEnding + FIXTURE_HEADER + LineEnding +
    ' .text 0x10001000 0x1000 system.o' + LineEnding +
    ' ' + FIXTURE_SECTION + LineEnding +
    ' 0x10001000 0x30 system.o' + LineEnding +
    ' 0x10001000 SYSTEM_$$_NULLHANDLER$crc12345678' + LineEnding;
end;

function TFPCSafeSEHTests.Resolve(const AManifest, AMap: string;
  const AVersion: string): TSERvaArray;
begin
  WriteText(FManifestFile, AManifest);
  WriteText(FMapFile, AMap);
  Result := ResolveFPCSafeSEH(FImage, FMapFile, FManifestFile, AVersion);
end;

procedure TFPCSafeSEHTests.Reject(const AManifest, AMap, AReason: string;
  const AVersion: string);
var
  Rejected: Boolean;
begin
  Rejected := False;
  try
    Resolve(AManifest, AMap, AVersion);
  except
    on E: ESafeSEH do
    begin
      Rejected := True;
      if Pos(AReason, string(E.Message)) = 0 then
        Fail('Expected ' + AReason + ', got ' + string(E.Message));
    end;
  end;
  Expect<Boolean>(Rejected).ToBe(True);
end;

procedure TFPCSafeSEHTests.SetupTests;
begin
  Test('resolves live section symbols without treating parameter types as names', TestValidMap);
  Test('requires every linked manifest handler', TestMissingHandler);
  Test('refuses unknown handler-like functions', TestUnknownHandler);
  Test('pins exact bytes of defining and registering objects', TestObjectMismatch);
  Test('requires linked toolchain objects after staging', TestMissingObject);
  Test('pins the queried full compiler version', TestVersionMismatch);
  Test('matches map and executable image bases', TestBaseMismatch);
  Test('optional units require their handlers only when linked', TestOptionalUnit);
  Test('accepts only explicit unreferenced-section removal', TestDeadStripping);
  Test('resolves discarded handlers in object paths containing parentheses', TestDeadStrippingParenthesizedPath);
  Test('rejects duplicate or conflicting map evidence', TestDuplicateEvidence);
  Test('rejects unsupported and malformed manifests', TestMalformedManifest);
  Test('rejects malformed or truncated handler addresses', TestMalformedMap);
  Test('refuses unreviewed toolchain inputs', TestUnreviewedInput);
  Test('binds handlers to the input object defining them', TestHandlerOwnership);
  Test('binds ordinary source exclusions to reviewed bytes and owner', TestSourceBinding);
  Test('validates source-bound receipts after project scratch cleanup', TestStagedProject);
  Test('requires source identity and build-owned output provenance', TestProjectProvenance);
  Test('refuses program, static method and nested handler definitions and aliases', TestProgramAndMethodNames);
end;

procedure TFPCSafeSEHTests.TestValidMap;
var
  Handlers: TSERvaArray;
begin
  Handlers := Resolve(Manifest, Map +
    ' .text.n_example_$$_ordinary$thandler' + LineEnding +
    ' 0x10001050 0x10 Example.o' + LineEnding +
    ' .rdata.n_VMT_$CLASSES_$$_TINITHANDLER 0x10003000 0x20 classes.o' + LineEnding +
    ' 0x10003000 VMT_$CLASSES_$$_TINITHANDLER' + LineEnding);
  Expect<Integer>(Length(Handlers)).ToBe(1);
  Expect<Cardinal>(Handlers[0]).ToBe(FIXTURE_RVA);
end;

procedure TFPCSafeSEHTests.TestMissingHandler;
begin
  Reject(Manifest, 'READOBJECT ' + FObjectFile + LineEnding + FIXTURE_HEADER,
    'Missing manifest handler');
end;

procedure TFPCSafeSEHTests.TestUnknownHandler;
begin
  Reject(Manifest, Map + ' .text.n_system_$$_new_exception_handler' + LineEnding +
    ' 0x10001040 0x10 system.o', 'Unreviewed handler-like');
  Reject(Manifest, Map + ' .text.n_external_$$_unknownhandler 0x10001040 0x10 external.o',
    'Unreviewed handler-like');
  Reject(Manifest, Map + ' .text.n_system_$$_unexpected 0x10001080 0x10 system.o' +
    LineEnding + ' 0x10001080 __FPC_unknown_handler', 'Unreviewed handler-like map symbol');
  Reject(StringReplace(Manifest, 'handler rtl/system.o ' + FIXTURE_SECTION + LineEnding,
    '', []), Map, 'Unreviewed handler-like');
end;

procedure TFPCSafeSEHTests.TestObjectMismatch;
begin
  WriteText(FObjectFile, 'unreviewed object bytes');
  try
    Reject(Manifest, Map, 'SHA-256 mismatch');
  finally
    WriteText(FObjectFile, 'reviewed object bytes');
  end;
  // sysinitpas has no handler definition but starts the registration path.
  // An object row is checked even when no handler row belongs to it.
  Reject(Manifest + 'object rtl/sysinitpas.o ' + StringOfChar('0', 64) + LineEnding,
    StringReplace(Map, FIXTURE_HEADER,
      'READOBJECT ' + FDirectory + PathDelim + 'sysinitpas.o' + LineEnding + FIXTURE_HEADER, []),
    'object unavailable');
end;

procedure TFPCSafeSEHTests.TestMissingObject;
begin
  DeleteFile(FObjectFile);
  try
    Reject(Manifest, Map, 'object unavailable');
  finally
    WriteText(FObjectFile, 'reviewed object bytes');
  end;
  Reject(Manifest, StringReplace(Map, 'READOBJECT ' + FObjectFile + LineEnding, '', []),
    'Missing required toolchain object');
end;

procedure TFPCSafeSEHTests.TestVersionMismatch;
begin
  Reject(Manifest, Map, 'FPC version differs', '3.2.4');
  Reject(Manifest, Map, 'FPC version differs', '3.2.4-rc2+2');
  Reject(Manifest, Map, 'FPC version differs', '');
end;

procedure TFPCSafeSEHTests.TestBaseMismatch;
begin
  Reject(Manifest, StringReplace(Map, FIXTURE_HEADER,
    'Memory map (ImageBase=0x00400000)', []), 'image base differs');
  Reject(Manifest, StringReplace(Map, FIXTURE_HEADER, '', []), 'Missing memory map');
end;

procedure TFPCSafeSEHTests.TestOptionalUnit;
var
  LinkedMap: string;
  Handlers: TSERvaArray;
begin
  Handlers := Resolve(Manifest, Map);
  Expect<Integer>(Length(Handlers)).ToBe(1);
  LinkedMap := StringReplace(Map, FIXTURE_HEADER,
    'READOBJECT ' + FOptionalFile + LineEnding + FIXTURE_HEADER, []);
  Reject(Manifest, LinkedMap, 'Missing manifest handler');
  LinkedMap := LinkedMap + ' ' + OPTIONAL_SECTION + LineEnding + ' 0x10001100 0x40 signals.o';
  Handlers := Resolve(Manifest, LinkedMap);
  Expect<Integer>(Length(Handlers)).ToBe(2);
  Expect<Cardinal>(Handlers[1]).ToBe($1100);
end;

procedure TFPCSafeSEHTests.TestDeadStripping;
var
  Handlers: TSERvaArray;
  LinkedMap: string;
begin
  LinkedMap := StringReplace(Map, FIXTURE_HEADER,
    'READOBJECT ' + FOptionalFile + LineEnding +
    'Removing signals.o(' + OPTIONAL_SECTION + ')' + LineEnding + FIXTURE_HEADER, []);
  Handlers := Resolve(Manifest, LinkedMap);
  Expect<Integer>(Length(Handlers)).ToBe(1);
  Reject(Manifest, LinkedMap + ' ' + OPTIONAL_SECTION + LineEnding +
    ' 0x10001100 0x40 signals.o', 'both live and discarded');
  Reject(Manifest, StringReplace(LinkedMap, 'Removing signals.o', 'Keeping signals.o', []),
    'Missing manifest handler');
end;

procedure TFPCSafeSEHTests.TestDeadStrippingParenthesizedPath;
var
  Directory, ObjectFile, LinkedMap: string;
  Handlers: TSERvaArray;
begin
  Directory := FDirectory + PathDelim + 'Program Files (x86)';
  ObjectFile := Directory + PathDelim + 'signals.o';
  ForceDirectories(Directory);
  WriteText(ObjectFile, 'reviewed object bytes');
  try
    LinkedMap := StringReplace(Map, FIXTURE_HEADER,
      'READOBJECT ' + ObjectFile + LineEnding +
      'Removing ' + ObjectFile + '(' + OPTIONAL_SECTION + ')' + LineEnding + FIXTURE_HEADER, []);
    Handlers := Resolve(Manifest, LinkedMap);
    Expect<Integer>(Length(Handlers)).ToBe(1);
    Expect<Cardinal>(Handlers[0]).ToBe(FIXTURE_RVA);
    // Also cover the Windows spelling when this test runs on another host.
    LinkedMap := StringReplace(LinkedMap, 'Removing ' + ObjectFile,
      'Removing C:\Program Files (x86)\FPC (pinned)\signals.o', []);
    Handlers := Resolve(Manifest, LinkedMap);
    Expect<Integer>(Length(Handlers)).ToBe(1);
    Reject(Manifest, LinkedMap + ' ' + OPTIONAL_SECTION + LineEnding +
      ' 0x10001100 0x40 signals.o', 'both live and discarded');
  finally
    DeleteFile(ObjectFile);
    RemoveDir(Directory);
  end;
end;

procedure TFPCSafeSEHTests.TestDuplicateEvidence;
begin
  Reject(Manifest, Map + LineEnding + FIXTURE_HEADER, 'Invalid memory map header');
  Reject(Manifest, Map + ' ' + FIXTURE_SECTION + LineEnding + ' 0x10001000 0x30 system.o',
    'Duplicate live');
  Reject(Manifest, 'READOBJECT ' + FObjectFile + LineEnding + Map, 'Duplicate linked');
end;

procedure TFPCSafeSEHTests.TestMalformedManifest;
begin
  Reject(StringReplace(Manifest, 'FPC-SAFESEH 1', 'FPC-SAFESEH 2', []), Map, 'Unsupported');
  Reject(Manifest + 'version ' + FIXTURE_VERSION, Map, 'Invalid manifest version');
  Reject(Manifest + 'target x86_64-win64', Map, 'Invalid manifest target');
  Reject(Manifest + 'object rtl/other.o not-a-digest', Map, 'Invalid manifest SHA-256');
  Reject(Manifest + 'handler rtl/system.o ' + FIXTURE_SECTION, Map, 'Duplicate manifest symbol');
  Reject(Manifest + 'unknown directive', Map, 'Unknown manifest directive');
end;

procedure TFPCSafeSEHTests.TestMalformedMap;
begin
  Reject(Manifest, StringReplace(Map, '0x10001000 0x30', '0x100001000 0x30', []), 'Invalid map address');
  Reject(Manifest, StringReplace(Map, '0x10001000 0x30', '0x00001000 0x30', []), 'Invalid live handler');
  Reject(Manifest, StringReplace(Map, '0x10001000 0x30', '0x10001000 0x0', []), 'Invalid live handler');
  Reject(Manifest, StringReplace(Map, '0x10001000 0x30', 'invalid 0x30', []), 'Malformed handler');
  Reject(Manifest, Map + ' ' + FIXTURE_SECTION, 'Truncated memory map');
end;

procedure TFPCSafeSEHTests.TestUnreviewedInput;
begin
  Reject(Manifest, 'READOBJECT /compiler/units/i386-win32/rtl/unknown.o' + LineEnding + Map,
    'Unreviewed linked input');
end;

procedure TFPCSafeSEHTests.TestHandlerOwnership;
begin
  Reject(Manifest, StringReplace(Map, '0x30 system.o', '0x30 other.o', []),
    'Unreviewed handler-like');
  Reject(Manifest, Map + ' ' + OPTIONAL_SECTION + LineEnding + ' 0x10001100 0x40 signals.o',
    'no linked input object');
end;

procedure TFPCSafeSEHTests.TestSourceBinding;
const
  SourceText: UTF8String = 'procedure OrdinaryHandler; begin end;';
  SourceSection = '.text.n_ordinary_$$_ordinaryhandler';
var
  SourcePath, ReviewedManifest, SourceMap: string;
  Bytes: TBytes;
  Entries: TSERvaArray;
begin
  SourcePath := FDirectory + PathDelim + 'source' + PathDelim + 'shared' + PathDelim + 'ordinary.pas';
  SetLength(Bytes, Length(SourceText));
  Move(SourceText[1], Bytes[0], Length(SourceText));
  ReviewedManifest := Manifest + 'source-nonseh ../../source/shared/ordinary.pas ' +
    SHA256Hex(Bytes) + ' ' + SourceSection + LineEnding;
  SourceMap := Map + ' ' + SourceSection + LineEnding + ' 0x10001100 0x10 ordinary.o';
  WriteText(SourcePath, string(SourceText));
  try
    Entries := Resolve(ReviewedManifest, SourceMap);
    Expect<Integer>(Length(Entries)).ToBe(1);
    Reject(Manifest, SourceMap, 'Unreviewed handler-like');
    Reject(ReviewedManifest, StringReplace(SourceMap, '0x10 ordinary.o', '0x10 different.o', []),
      'Unreviewed handler-like');
    WriteText(SourcePath, 'changed source');
    Reject(ReviewedManifest, SourceMap, 'source SHA-256 mismatch');
    DeleteFile(SourcePath);
    Reject(ReviewedManifest, SourceMap, 'source unavailable');
  finally
    DeleteFile(SourcePath);
  end;
end;

procedure TFPCSafeSEHTests.TestStagedProject;
var
  Entries: TSERvaArray;
  Source, ObjectPath, ProjectMap, Receipt, OriginalSource, ObjectDirectory: string;
begin
  Source := FDirectory + PathDelim + 'source' + PathDelim + 'shared' + PathDelim + 'project.pas';
  ObjectDirectory := FDirectory + PathDelim + 'compiled';
  ObjectPath := ObjectDirectory + PathDelim + 'project.o';
  OriginalSource := 'unit project; interface implementation end.';
  WriteText(Source, OriginalSource);
  WriteText(ObjectPath, 'compiled project bytes');
  ProjectMap := 'READOBJECT ' + ObjectPath + LineEnding + Map;
  WriteText(FManifestFile, Manifest);
  WriteText(FMapFile, ProjectMap);
  try
    Entries := ResolveFPCSafeSEH(FImage, FMapFile, FManifestFile,
      FIXTURE_VERSION, ObjectDirectory);
    Expect<Integer>(Length(Entries)).ToBe(1);
    Reject(Manifest, ProjectMap, 'Missing build-owned input provenance');
    Receipt := 'FPC-SAFESEH-INPUTS 1' + LineEnding +
      'map ' + SHA256Hex(ReadFileBytes(FMapFile)) + LineEnding +
      'image ' + SHA256Hex(FImage) + LineEnding +
      'version ' + FIXTURE_VERSION + LineEnding +
      'cwd ' + GetCurrentDir + LineEnding +
      'directory ' + CanonicalHostPath(ObjectDirectory) + LineEnding +
      ObjectPath + #9 + Source + #9 + SHA256Hex(ReadFileBytes(Source)) + #9 +
      SHA256Hex(ReadFileBytes(ObjectPath)) + #9 + CanonicalHostPath(ObjectPath) + LineEnding;
    WriteText(FMapFile + '.safeseh-inputs', Receipt);
    Entries := Resolve(Manifest, ProjectMap);
    Expect<Integer>(Length(Entries)).ToBe(1);
    WriteText(ObjectPath, 'substituted object');
    Reject(Manifest, ProjectMap, 'Project object SHA-256 differs');
    DeleteFile(ObjectPath);
    RemoveDir(ObjectDirectory);
    Entries := Resolve(Manifest, ProjectMap);
    Expect<Integer>(Length(Entries)).ToBe(1);
    WriteText(Source, 'changed source');
    Reject(Manifest, ProjectMap, 'Project source SHA-256 differs');
    WriteText(Source, OriginalSource);
    Reject(Manifest, ProjectMap + LineEnding, 'receipt map SHA-256 mismatch');
    FImage[0] := 1;
    try
      Reject(Manifest, ProjectMap, 'receipt image SHA-256 mismatch');
    finally
      FImage[0] := 0;
    end;
  finally
    DeleteFile(FMapFile + '.safeseh-inputs');
    DeleteFile(ObjectPath);
    DeleteFile(Source);
    ForceDirectories(ObjectDirectory);
  end;
end;

procedure TFPCSafeSEHTests.TestProjectProvenance;
var
  Source, ObjectPath, ProjectMap: string;
  Rejected: Boolean;
begin
  ObjectPath := FDirectory + PathDelim + 'compiled' + PathDelim + 'project.o';
  Source := FDirectory + PathDelim + 'source' + PathDelim + 'shared' + PathDelim + 'project.pas';
  ProjectMap := 'READOBJECT ' + ObjectPath + LineEnding + Map;
  WriteText(ObjectPath, 'an arbitrary object');
  try
    Reject(Manifest, ProjectMap, 'without project source');
    WriteText(Source, 'unit project; interface implementation end.');
    Reject(Manifest, ProjectMap, 'Missing build-owned input provenance');
    Rejected := False;
    try
      ResolveFPCSafeSEH(FImage, FMapFile, FManifestFile, FIXTURE_VERSION, FDirectory);
    except
      on E: ESafeSEH do
      begin
        Rejected := Pos('outside the build-owned', string(E.Message)) > 0;
      end;
    end;
    Expect<Boolean>(Rejected).ToBe(True);
    Reject(Manifest, 'READOBJECT alternate/inifiles.o' + LineEnding + Map,
      'without project source');
  finally
    DeleteFile(Source);
    DeleteFile(ObjectPath);
  end;
end;

procedure TFPCSafeSEHTests.TestProgramAndMethodNames;
const
  NAMES: array[0..3] of string = (
    'p$topentry_$$_exceptionhandler$pointer$pointer$pointer$pointer$$longint',
    'p$classentry$_$tprobe_$__$$_exceptionhandler$pointer$pointer$pointer$pointer$$longint',
    'example$_$outer$pointer_$$_nestedhandler$pointer',
    'p$nestedentry_$$_outer_$$_exceptionhandler$pointer$$longint');
var
  Name: string;
begin
  for Name in NAMES do
  begin
    Reject(Manifest, Map + ' .text.n_' + Name + LineEnding +
      ' 0x10001080 0x10 project.o', 'Unreviewed handler-like map section');
    Reject(Manifest, Map + ' .text.n_example_$$_ordinary 0x10001080 0x10 project.o' +
      LineEnding + ' 0x10001080 ' + UpperCase(Name), 'Unreviewed handler-like map symbol');
  end;
end;

begin
  TestRunnerProgram.AddSuite(TFPCSafeSEHTests.Create);
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
