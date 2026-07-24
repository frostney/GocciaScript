program GocciaJSON5ComplianceRunner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Math,
  StrUtils,
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

type
  TJSON5CaseData = record
    ID: string;
    Valid: Boolean;
    TestPath: string;
  end;

  TJSON5CaseDataArray = array of TJSON5CaseData;

  TJSON5ComplianceExecutor = class(TComplianceCaseExecutor)
  private
    FCases: TJSON5CaseDataArray;
    FTestRunner: string;
    FTimeoutMilliseconds: Integer;
  public
    constructor Create(const ACases: TJSON5CaseDataArray;
      const ATestRunner: string; const ATimeoutMilliseconds: Integer);
    procedure ExecuteCase(const ACaseID: string; const AIndex: Integer;
      out AResult: TComplianceCaseResult); override;
  end;

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
      '  let actual;' + LineEnding +
      '  try {' + LineEnding +
      '    actual = JSON5.parse(source);' + LineEnding +
      '  } catch (error) {' + LineEnding +
      '    throw new Error("__GOCCIA_FALSE_REJECT__ " + error);' + LineEnding +
      '  }' + LineEnding +
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

constructor TJSON5ComplianceExecutor.Create(const ACases: TJSON5CaseDataArray;
  const ATestRunner: string; const ATimeoutMilliseconds: Integer);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FCases, Length(ACases));
  for I := 0 to High(ACases) do
    FCases[I] := ACases[I];
  FTestRunner := ATestRunner;
  FTimeoutMilliseconds := ATimeoutMilliseconds;
end;

procedure TJSON5ComplianceExecutor.ExecuteCase(const ACaseID: string;
  const AIndex: Integer; out AResult: TComplianceCaseResult);
var
  Arguments: TStringList;
  ProcessResult: TComplianceProcessResult;
begin
  AResult.ID := ACaseID;
  AResult.Valid := FCases[AIndex].Valid;
  AResult.Outcome := coInfrastructure;
  Arguments := TStringList.Create;
  try
    Arguments.Add(FCases[AIndex].TestPath);
    Arguments.Add('--silent');
    Arguments.Add('--no-progress');
    Arguments.Add('--no-results');
    ProcessResult := RunComplianceProcess(FTestRunner, Arguments,
      FTimeoutMilliseconds);
  finally
    Arguments.Free;
  end;
  AResult.DurationMilliseconds := ProcessResult.DurationMilliseconds;
  AResult.MessageText := CombinedProcessMessage(ProcessResult);
  if not ProcessResult.Started then
    Exit;
  if ProcessResult.TimedOut then
  begin
    AResult.Outcome := coTimeout;
    AResult.MessageText := 'timeout';
    Exit;
  end;
  if ProcessResult.ExitCode = 0 then
  begin
    AResult.Outcome := coPassed;
    AResult.MessageText := '';
  end
  else if ProcessResult.ExitCode = 1 then
  begin
    if not AResult.Valid then
      AResult.Outcome := coFalseAccept
    else if Pos('__GOCCIA_FALSE_REJECT__', AResult.MessageText) > 0 then
      AResult.Outcome := coFalseReject
    else
      AResult.Outcome := coMismatch;
  end
  else
    AResult.Outcome := coCrash;
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
  const ACases: TJSON5CaseDataArray);
var
  I: Integer;
begin
  for I := 0 to High(ACases) do
    if StartsText(IncludeTrailingPathDelimiter(ADirectory),
        ExpandFileName(ACases[I].TestPath)) then
      DeleteFile(ACases[I].TestPath);
  RemoveDir(ADirectory);
end;

function LoadCases(const AManifestPath, AExpectedRevision,
  ACaseDirectory: string; out ACases: TJSON5CaseDataArray): TStringList;
var
  CaseArray: TGocciaArrayValue;
  CaseObject, ManifestObject: TGocciaObjectValue;
  ExpectedValue: TGocciaValue;
  I: Integer;
  JSONParser: TGocciaJSONParser;
  JSONStringifier: TGocciaJSONStringifier;
  SourceText, ExpectedJSON: string;
begin
  if not FileExists(AManifestPath) then
    raise Exception.CreateFmt(
      'JSON5 manifest not found: %s (run --regenerate-manifest first)',
      [AManifestPath]);
  JSONParser := TGocciaJSONParser.Create;
  JSONStringifier := TGocciaJSONStringifier.Create;
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
    SetLength(ACases, CaseArray.Elements.Count);
    Result := TStringList.Create;
    for I := 0 to CaseArray.Elements.Count - 1 do
    begin
      CaseObject := TGocciaObjectValue(CaseArray.Elements[I]);
      ACases[I].ID := CaseObject.GetProperty('id').ToStringLiteral.Value;
      ACases[I].Valid := CaseObject.GetProperty('valid').ToBooleanLiteral.Value;
      ACases[I].TestPath := IncludeTrailingPathDelimiter(ACaseDirectory) +
        Format('case_%.4d.js', [I]);
      SourceText := CaseObject.GetProperty('source').ToStringLiteral.Value;
      ExpectedJSON := '';
      if ACases[I].Valid then
      begin
        ExpectedValue := CaseObject.GetProperty('expected');
        ExpectedJSON := JSONStringifier.Stringify(ExpectedValue);
      end;
      WriteUTF8FileText(ACases[I].TestPath,
        GeneratedCaseSource(ACases[I].ID, SourceText, ExpectedJSON,
          ACases[I].Valid));
      Result.Add(ACases[I].ID);
    end;
  finally
    JSONStringifier.Free;
    JSONParser.Free;
  end;
end;

function RunStringifySuite(const ATestRunner, ASuitePath,
  AReportPath: string; const ATimeoutMilliseconds: Integer;
  out AReportJSON: string): TComplianceCaseResult;
var
  Arguments: TStringList;
  ProcessResult: TComplianceProcessResult;
begin
  Result.ID := 'JSON5.stringify';
  Result.Valid := True;
  Result.Outcome := coInfrastructure;
  Arguments := TStringList.Create;
  try
    Arguments.Add(ASuitePath);
    Arguments.Add('--silent');
    Arguments.Add('--no-progress');
    Arguments.Add('--output=' + AReportPath);
    ProcessResult := RunComplianceProcess(ATestRunner, Arguments,
      ATimeoutMilliseconds);
  finally
    Arguments.Free;
  end;
  Result.DurationMilliseconds := ProcessResult.DurationMilliseconds;
  Result.MessageText := CombinedProcessMessage(ProcessResult);
  if ProcessResult.TimedOut then
  begin
    Result.Outcome := coTimeout;
    Result.MessageText := 'timeout';
  end
  else if not ProcessResult.Started then
    Result.Outcome := coInfrastructure
  else if ProcessResult.ExitCode = 0 then
  begin
    Result.Outcome := coPassed;
    Result.MessageText := '';
  end
  else if ProcessResult.ExitCode = 1 then
    Result.Outcome := coMismatch
  else
    Result.Outcome := coCrash;
  if FileExists(AReportPath) then
    AReportJSON := ReadUTF8FileText(AReportPath)
  else
    AReportJSON := 'null';
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
  WriteLn('  --jobs=N                Maximum concurrent TestRunner processes');
  WriteLn('  --timeout=N             Per-process timeout in milliseconds');
  WriteLn('  --output=PATH           Write the complete JSON report');
  WriteLn('  --regenerate-manifest   Maintainer-only manifest regeneration');
end;

function RunCoordinator: Integer;
var
  CaseDirectory, ManifestPath, OutputPath, PinPath, Revision,
  StringifyReportJSON, StringifySuite, SuiteDirectory, TestRunner: string;
  Cases: TJSON5CaseDataArray;
  StringifyResults: TComplianceCaseResultArray;
  CaseIDs: TStringList;
  Coordinator: TComplianceCoordinator;
  Executor: TJSON5ComplianceExecutor;
  I, Jobs, TimeoutMilliseconds: Integer;
  AllResults: TComplianceCaseResultArray;
  ParseResults: TComplianceCaseResultArray;
  AllSummary, ParseSummary: TComplianceSummary;
  ReportJSON, StringifyReportPath: string;
  StartedAt, DurationMilliseconds: QWord;
  StringifyResult: TComplianceCaseResult;
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

  CaseDirectory := CreateCaseDirectory;
  StringifyReportPath := IncludeTrailingPathDelimiter(CaseDirectory) +
    'stringify-report.json';
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    CaseIDs := LoadCases(ExpandFileName(ManifestPath), Revision,
      CaseDirectory, Cases);
    Executor := TJSON5ComplianceExecutor.Create(Cases, TestRunner,
      TimeoutMilliseconds);
    Coordinator := TComplianceCoordinator.Create(Executor);
    StartedAt := GetTickCount64;
    try
      ParseResults := Coordinator.Run(CaseIDs, Jobs);
    finally
      Coordinator.Free;
      Executor.Free;
      CaseIDs.Free;
    end;
    StringifyResult := RunStringifySuite(TestRunner, StringifySuite,
      StringifyReportPath, TimeoutMilliseconds, StringifyReportJSON);
    SetLength(StringifyResults, 1);
    StringifyResults[0] := StringifyResult;
    SetLength(AllResults, Length(ParseResults) + 1);
    for I := 0 to High(ParseResults) do
      AllResults[I] := ParseResults[I];
    AllResults[High(AllResults)] := StringifyResult;
    DurationMilliseconds := GetTickCount64 - StartedAt;
    ParseSummary := SummarizeComplianceResults(ParseResults);
    AllSummary := SummarizeComplianceResults(AllResults);
    ReportJSON := Format(
      '{"reportVersion":%d,"runner":{"name":%s,"version":%d},' +
      '"suite":{"name":%s,"revision":%s},' +
      '"mode":"testrunner","jobs":%d,' +
      '"timing":{"durationMilliseconds":%d,"timeoutMilliseconds":%d},' +
      '"summary":%s,"cases":%s,' +
      '"sections":{"parse":{"summary":%s},' +
      '"stringify":{"result":%s,"testRunnerReport":%s}}}',
      [COMPLIANCE_REPORT_VERSION,
       QuoteJSONString('GocciaJSON5ComplianceRunner'),
       COMPLIANCE_RUNNER_VERSION, QuoteJSONString(SUITE_NAME),
       QuoteJSONString(Revision), Min(Jobs, Max(1, ParseSummary.Total)),
       DurationMilliseconds, TimeoutMilliseconds,
       ComplianceSummaryJSON(AllSummary), ComplianceCasesJSON(AllResults),
       ComplianceSummaryJSON(ParseSummary),
       Copy(ComplianceCasesJSON(StringifyResults), 2,
         Length(ComplianceCasesJSON(StringifyResults)) - 2),
       Trim(StringifyReportJSON)]);
    if OutputPath <> '' then
      WriteJSONFile(OutputPath, ReportJSON);
    WriteLn('JSON5 parse: ', ParseSummary.Passed, '/', ParseSummary.Total,
      ' passed; stringify: ',
      ComplianceOutcomeName(StringifyResult.Outcome));
    WriteLn(ComplianceSummaryJSON(AllSummary));
  finally
    TGarbageCollector.Shutdown;
    DeleteFile(StringifyReportPath);
    RemoveCaseDirectory(CaseDirectory, Cases);
  end;
  if (ParseSummary.Failed > 0) or
     (StringifyResult.Outcome <> coPassed) then
    Result := 1
  else
    Result := 0;
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
