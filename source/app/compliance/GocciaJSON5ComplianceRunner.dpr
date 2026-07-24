program GocciaJSON5ComplianceRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  Goccia.Compliance,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.JSON.Utils,
  Goccia.TextFiles,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  FileUtils;

const
  SUITE_NAME = 'json5';
  DEFAULT_PIN_PATH = 'tests/compliance/json5.pin';
  DEFAULT_MANIFEST_NAME = 'goccia-json5-cases.json';
  DEFAULT_REGENERATOR = 'scripts/regenerate-json5-manifest.js';
  DEFAULT_STRINGIFY_SUITE = 'tests/built-ins/JSON5/stringify.js';

function GeneratedCaseSource(const AID, ASource, AExpectedJSON: string;
  const AValid: Boolean): string;
const
  ENCODER_SOURCE =
    'const encode = (value) => {' + LineEnding +
    '  if (value === null) return { type: "null" };' + LineEnding +
    '  if (typeof value === "boolean") return { type: "boolean", value };' +
      LineEnding +
    '  if (typeof value === "string") return { type: "string", value };' +
      LineEnding +
    '  if (typeof value === "number") {' + LineEnding +
    '    if (Number.isNaN(value)) return { type: "number", value: "NaN" };' +
      LineEnding +
    '    if (value === Infinity) return { type: "number", value: "Infinity" };' +
      LineEnding +
    '    if (value === -Infinity) return { type: "number", value: "-Infinity" };' +
      LineEnding +
    '    if (Object.is(value, -0)) return { type: "number", value: "-0" };' +
      LineEnding +
    '    return { type: "number", value: String(value) };' + LineEnding +
    '  }' + LineEnding +
    '  if (Array.isArray(value)) {' + LineEnding +
    '    return { type: "array", items: value.map(item => encode(item)) };' +
      LineEnding +
    '  }' + LineEnding +
    '  return {' + LineEnding +
    '    type: "object",' + LineEnding +
    '    entries: Object.getOwnPropertyNames(value).map(key => ({' + LineEnding +
    '      key, value: encode(value[key]),' + LineEnding +
    '    })),' + LineEnding +
    '  };' + LineEnding +
    '};' + LineEnding;
var
  Body: string;
begin
  if AValid then
    Body :=
      '  const actual = JSON5.parse(source);' + LineEnding +
      '  expect(JSON.stringify(encode(actual))).toBe(' +
      'JSON.stringify(expected));' + LineEnding
  else
    Body := '  expect(() => JSON5.parse(source)).toThrow(SyntaxError);' +
      LineEnding;
  Result :=
    'import * as JSON5 from "goccia:json5";' + LineEnding + LineEnding +
    ENCODER_SOURCE + LineEnding +
    'test(' + QuoteJSONString(AID) + ', () => {' + LineEnding +
    '  const source = ' + QuoteJSONString(ASource) + ';' + LineEnding;
  if AValid then
    Result := Result + '  const expected = ' + AExpectedJSON + ';' + LineEnding;
  Result := Result + Body + '});' + LineEnding;
end;

function DefaultTestRunnerPath: string;
begin
  {$IFDEF WINDOWS}
  Result := 'build' + DirectorySeparator + 'GocciaTestRunner.exe';
  {$ELSE}
  Result := 'build' + DirectorySeparator + 'GocciaTestRunner';
  {$ENDIF}
end;

function CreateCaseDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-json5-compliance-' + IntToStr(GetProcessID) + '-' +
    IntToStr(GetTickCount64);
  if not ForceDirectories(Result) then
    raise Exception.CreateFmt('Cannot create temporary case directory: %s',
      [Result]);
end;

procedure RemoveCaseDirectory(const ADirectory: string;
  const ACasePaths: TStrings);
var
  I: Integer;
begin
  if ACasePaths <> nil then
    for I := 0 to ACasePaths.Count - 1 do
      DeleteFile(ACasePaths[I]);
  RemoveDir(ADirectory);
end;

function PrepareCases(const AManifestPath, AExpectedRevision,
  ACaseDirectory: string): TStringList;
var
  CaseArray: TGocciaArrayValue;
  CaseObject, ManifestObject: TGocciaObjectValue;
  ExpectedValue: TGocciaValue;
  I: Integer;
  JSONParser: TGocciaJSONParser;
  JSONStringifier: TGocciaJSONStringifier;
  CasePath, ExpectedJSON: string;
begin
  if not FileExists(AManifestPath) then
    raise Exception.CreateFmt(
      'JSON5 manifest not found: %s (run --regenerate-manifest first)',
      [AManifestPath]);
  Result := TStringList.Create;
  JSONParser := TGocciaJSONParser.Create;
  JSONStringifier := TGocciaJSONStringifier.Create;
  try
    try
      ManifestObject := TGocciaObjectValue(JSONParser.Parse(
        ReadUTF8FileText(AManifestPath)));
      if ManifestObject.GetProperty('manifestVersion').ToNumberLiteral.Value <>
          1 then
        raise Exception.Create('Unsupported JSON5 manifest version');
      if ManifestObject.GetProperty('suite').ToStringLiteral.Value <>
          SUITE_NAME then
        raise Exception.Create('JSON5 manifest names the wrong suite');
      if ManifestObject.GetProperty('revision').ToStringLiteral.Value <>
          AExpectedRevision then
        raise Exception.CreateFmt(
          'JSON5 manifest revision mismatch: expected %s, got %s',
          [AExpectedRevision,
           ManifestObject.GetProperty('revision').ToStringLiteral.Value]);
      CaseArray := TGocciaArrayValue(ManifestObject.GetProperty('cases'));
      for I := 0 to CaseArray.Elements.Count - 1 do
      begin
        CaseObject := TGocciaObjectValue(CaseArray.Elements[I]);
        CasePath := IncludeTrailingPathDelimiter(ACaseDirectory) +
          Format('case_%.4d.js', [I]);
        ExpectedJSON := '';
        if CaseObject.GetProperty('valid').ToBooleanLiteral.Value then
        begin
          ExpectedValue := CaseObject.GetProperty('expected');
          ExpectedJSON := JSONStringifier.Stringify(ExpectedValue);
        end;
        WriteUTF8FileText(CasePath, GeneratedCaseSource(
          CaseObject.GetProperty('id').ToStringLiteral.Value,
          CaseObject.GetProperty('source').ToStringLiteral.Value,
          ExpectedJSON,
          CaseObject.GetProperty('valid').ToBooleanLiteral.Value));
        Result.Add(CasePath);
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    JSONStringifier.Free;
    JSONParser.Free;
  end;
end;

function RegenerateManifest: Integer;
var
  Arguments: TStringList;
  ManifestPath, NodeExecutable, PinPath, RegeneratorPath, Revision,
  SuiteDirectory: string;
  ProcessResult: TComplianceProcessResult;
begin
  if not ReadOptionValue('suite-dir', SuiteDirectory) then
    raise Exception.Create('--suite-dir is required');
  SuiteDirectory := ExpandFileName(SuiteDirectory);
  if not ReadOptionValue('pin-file', PinPath) then
    PinPath := DEFAULT_PIN_PATH;
  Revision := ReadRequiredPin(PinPath);
  VerifySuiteRevision(SuiteDirectory, Revision);
  if not ReadOptionValue('manifest', ManifestPath) then
    ManifestPath := IncludeTrailingPathDelimiter(SuiteDirectory) +
      DEFAULT_MANIFEST_NAME;
  if not ReadOptionValue('node', NodeExecutable) then
    NodeExecutable := 'node';
  if not ReadOptionValue('regenerator', RegeneratorPath) then
    RegeneratorPath := DEFAULT_REGENERATOR;
  Arguments := TStringList.Create;
  try
    Arguments.Add(ExpandFileName(RegeneratorPath));
    Arguments.Add(SuiteDirectory);
    Arguments.Add(Revision);
    Arguments.Add(ExpandFileName(ManifestPath));
    ProcessResult := RunComplianceProcess(NodeExecutable, Arguments, 60000);
  finally
    Arguments.Free;
  end;
  if (not ProcessResult.Started) or (ProcessResult.ExitCode <> 0) then
    raise Exception.Create('Manifest regeneration failed: ' +
      CombinedProcessMessage(ProcessResult));
  WriteLn(Trim(ProcessResult.StdOutText));
  Result := 0;
end;

procedure PrintUsage;
begin
  WriteLn('Usage: GocciaJSON5ComplianceRunner --suite-dir=PATH [options]');
  WriteLn('  --manifest=PATH         Prepared, revision-tagged case manifest');
  WriteLn('  --test-runner=PATH      GocciaTestRunner executable');
  WriteLn('  --stringify-suite=PATH  Local JSON5 stringify test file');
  WriteLn('  --pin-file=PATH         Expected suite revision pin');
  WriteLn('  --jobs=N                TestRunner worker count');
  WriteLn('  --timeout=N             TestRunner per-file timeout in milliseconds');
  WriteLn('  --output=PATH           Write the TestRunner JSON report');
  WriteLn('  --regenerate-manifest   Maintainer-only manifest regeneration');
end;

function RunCoordinator: Integer;
var
  Arguments, CasePaths: TStringList;
  CaseDirectory, ManifestPath, OutputPath, PinPath, Revision,
  StringifySuite, SuiteDirectory, TestRunner: string;
  Jobs, TimeoutMilliseconds: Integer;
  ProcessResult: TComplianceProcessResult;
begin
  if not ReadOptionValue('suite-dir', SuiteDirectory) then
  begin
    PrintUsage;
    Exit(2);
  end;
  SuiteDirectory := ExpandFileName(SuiteDirectory);
  if not ReadOptionValue('pin-file', PinPath) then
    PinPath := DEFAULT_PIN_PATH;
  Revision := ReadRequiredPin(PinPath);
  VerifySuiteRevision(SuiteDirectory, Revision);
  if not ReadOptionValue('manifest', ManifestPath) then
    ManifestPath := IncludeTrailingPathDelimiter(SuiteDirectory) +
      DEFAULT_MANIFEST_NAME;
  if not ReadOptionValue('test-runner', TestRunner) then
    TestRunner := DefaultTestRunnerPath;
  TestRunner := ExpandFileName(TestRunner);
  if not FileExists(TestRunner) then
    raise Exception.CreateFmt('GocciaTestRunner not found: %s', [TestRunner]);
  if not ReadOptionValue('stringify-suite', StringifySuite) then
    StringifySuite := DEFAULT_STRINGIFY_SUITE;
  StringifySuite := ExpandFileName(StringifySuite);
  if not FileExists(StringifySuite) then
    raise Exception.CreateFmt('JSON5 stringify suite not found: %s',
      [StringifySuite]);
  Jobs := PositiveIntegerOption('jobs', DefaultComplianceJobs);
  TimeoutMilliseconds := PositiveIntegerOption('timeout',
    DEFAULT_COMPLIANCE_TIMEOUT_MS);
  ReadOptionValue('output', OutputPath);
  if (OutputPath <> '') and
     (not ForceDirectories(ExtractFileDir(ExpandFileName(OutputPath)))) then
    raise Exception.CreateFmt('Cannot create report directory: %s',
      [ExtractFileDir(ExpandFileName(OutputPath))]);

  CaseDirectory := CreateCaseDirectory;
  CasePaths := nil;
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    CasePaths := PrepareCases(ExpandFileName(ManifestPath), Revision,
      CaseDirectory);
    Arguments := TStringList.Create;
    try
      Arguments.Add(CaseDirectory);
      Arguments.Add(StringifySuite);
      Arguments.Add('--jobs=' + IntToStr(Jobs));
      Arguments.Add('--timeout=' + IntToStr(TimeoutMilliseconds));
      Arguments.Add('--silent');
      Arguments.Add('--no-progress');
      if OutputPath <> '' then
        Arguments.Add('--output=' + ExpandFileName(OutputPath));
      ProcessResult := RunComplianceProcess(TestRunner, Arguments, 0);
    finally
      Arguments.Free;
    end;
  finally
    TGarbageCollector.Shutdown;
    RemoveCaseDirectory(CaseDirectory, CasePaths);
    CasePaths.Free;
  end;
  if not ProcessResult.Started then
    raise Exception.Create('Cannot start GocciaTestRunner: ' +
      ProcessResult.ErrorMessage);
  if ProcessResult.StdOutText <> '' then
    Write(TrimRight(ProcessResult.StdOutText), LineEnding);
  if ProcessResult.StdErrText <> '' then
    Write(TrimRight(ProcessResult.StdErrText), LineEnding);
  Result := ProcessResult.ExitCode;
end;

begin
  try
    if HasCommandLineFlag('help') then
    begin
      PrintUsage;
      ExitCode := 0;
    end
    else if HasCommandLineFlag('regenerate-manifest') then
      ExitCode := RegenerateManifest
    else
      ExitCode := RunCoordinator;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
