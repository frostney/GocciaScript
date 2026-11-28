program GocciaYAMLComplianceRunner;

{ Runs TGocciaYAMLParser over a prepared checkout of the yaml-test-suite data
  branch (github.com/yaml/yaml-test-suite). Each case runs in a worker process
  of this executable. A valid case passes when it parses and, where the case
  has in.json, when its documents equal those JSON values; a case with an
  error file passes when the parser rejects it. Cases listed in the
  known-failures file may fail; the run fails on any other failure and on a
  listed case that passes or no longer exists. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Math,
  StrUtils,
  SysUtils,

  Goccia.Compliance,
  Goccia.DataTree,
  Goccia.DataTree.JSON,
  Goccia.EngineFault,
  {$IFDEF GOCCIA_FAULT_INJECTION}
  Goccia.FaultInjection,
  {$ENDIF}
  Goccia.JSON.Utils,
  Goccia.TextFiles,
  Goccia.YAML,

  NumericText,
  StringBuffer;

const
  SUITE_NAME = 'yaml-test-suite';
  SUITE_VERSION = '1.2.2';
  DEFAULT_PIN_PATH = 'tests/compliance/yaml-test-suite.pin';
  DEFAULT_KNOWN_FAILURES_PATH = 'tests/compliance/yaml-test-suite.known-failures';
  CASE_INPUT_FILE = 'in.yaml';
  CASE_VALUES_FILE = 'in.json';
  CASE_ERROR_FILE = 'error';
  { the data branch's indexes of the cases by name and by tag: symlinks to
    the case directories, so walking them runs each case again }
  INDEX_DIRECTORIES: array[0..1] of string = ('name', 'tags');

  WORKER_EXIT_PARSED = 0;
  WORKER_EXIT_REJECTED = 1;
  WORKER_EXIT_USAGE = 2;
  WORKER_EXIT_UNEXPECTED_EXCEPTION = 3;
  WORKER_EXIT_NO_JSON_FORM = 4;

type
  EYAMLNoJSONForm = class(Exception);

  TYAMLComplianceExecutor = class(TComplianceCaseExecutor)
  private
    FExecutable: string;
    FSuiteDirectory: string;
    FTimeoutMilliseconds: Integer;
  public
    constructor Create(const AExecutable, ASuiteDirectory: string;
      const ATimeoutMilliseconds: Integer);
    procedure ExecuteCase(const ACaseID: string; const AIndex: Integer;
      out AResult: TComplianceCaseResult); override;
  end;

function CaseDirectory(const ASuiteDirectory, ACaseID: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ASuiteDirectory) +
    StringReplace(ACaseID, '/', DirectorySeparator, [rfReplaceAll]);
end;

function CaseFile(const ACaseDirectory, AName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ACaseDirectory) + AName;
end;

{ AText as a JSON string literal in ASCII, every other code unit escaped, so
  the worker's output does not depend on the console's code page }
procedure AppendASCIIJSONString(var ABuffer: TStringBuffer; const AText: string);
const
  HEX_DIGITS: array[0..15] of Char = '0123456789abcdef';
var
  C: Char;
  I: Integer;
begin
  ABuffer.AppendChar('"');
  for I := 1 to Length(AText) do
  begin
    C := AText[I];
    case C of
      '"':
        ABuffer.Append('\"');
      '\':
        ABuffer.Append('\\');
      #10:
        ABuffer.Append('\n');
      #13:
        ABuffer.Append('\r');
      #9:
        ABuffer.Append('\t');
    else
      if (Ord(C) < 32) or (Ord(C) > 126) then
      begin
        ABuffer.Append('\u');
        ABuffer.AppendChar(HEX_DIGITS[(Ord(C) shr 12) and 15]);
        ABuffer.AppendChar(HEX_DIGITS[(Ord(C) shr 8) and 15]);
        ABuffer.AppendChar(HEX_DIGITS[(Ord(C) shr 4) and 15]);
        ABuffer.AppendChar(HEX_DIGITS[Ord(C) and 15]);
      end
      else
        ABuffer.AppendChar(C);
    end;
  end;
  ABuffer.AppendChar('"');
end;

{ ANode as plain JSON, a tagged node as its content and a node an alias
  shares written out wherever it stands. Raises EYAMLNoJSONForm for a value
  JSON cannot hold: a cycle, NaN or an infinity. }
procedure AppendPlainJSON(var ABuffer: TStringBuffer;
  const ANode: TGocciaDataNode; const APath: TGocciaDataNodeList);
var
  I: Integer;
  Pair: TGocciaDataNodeMap.TKeyValuePair;
begin
  if APath.IndexOf(ANode) >= 0 then
    raise EYAMLNoJSONForm.Create('the value contains itself');
  case ANode.Kind of
    dnkNull:
      ABuffer.Append('null');
    dnkBoolean:
      if ANode.BooleanValue then
        ABuffer.Append('true')
      else
        ABuffer.Append('false');
    dnkNumber:
      begin
        if IsNan(ANode.NumberValue) or IsInfinite(ANode.NumberValue) then
          raise EYAMLNoJSONForm.CreateFmt('the number %s has no JSON form',
            [NumberToString(ANode.NumberValue)]);
        ABuffer.Append(NumberToString(ANode.NumberValue));
      end;
    dnkString:
      AppendASCIIJSONString(ABuffer, ANode.StringValue);
    dnkTagged:
      AppendPlainJSON(ABuffer, ANode.TaggedValue, APath);
    dnkArray:
      begin
        APath.Add(ANode);
        ABuffer.AppendChar('[');
        for I := 0 to ANode.Count - 1 do
        begin
          if I > 0 then
            ABuffer.AppendChar(',');
          AppendPlainJSON(ABuffer, ANode.Items[I], APath);
        end;
        ABuffer.AppendChar(']');
        APath.Delete(APath.Count - 1);
      end;
    dnkObject:
      begin
        APath.Add(ANode);
        ABuffer.AppendChar('{');
        I := 0;
        for Pair in ANode.Properties do
        begin
          if I > 0 then
            ABuffer.AppendChar(',');
          AppendASCIIJSONString(ABuffer, Pair.Key);
          ABuffer.AppendChar(':');
          AppendPlainJSON(ABuffer, Pair.Value, APath);
          Inc(I);
        end;
        ABuffer.AppendChar('}');
        APath.Delete(APath.Count - 1);
      end;
  end;
end;

function PlainJSON(const ANode: TGocciaDataNode): string;
var
  Buffer: TStringBuffer;
  Path: TGocciaDataNodeList;
begin
  Buffer := TStringBuffer.Create;
  Path := TGocciaDataNodeList.Create;
  try
    AppendPlainJSON(Buffer, ANode, Path);
  finally
    Path.Free;
  end;
  Result := Buffer.ToString;
end;

{ The end, exclusive, of the JSON value that starts at AStart: a container to
  its matching bracket, a string to its closing quote, anything else to the
  next whitespace or bracket }
function JSONValueEnd(const AText: string; const AStart: Integer): Integer;
var
  Depth: Integer;
  InString: Boolean;
begin
  Result := AStart;
  if CharInSet(AText[Result], ['{', '[', '"']) then
  begin
    Depth := 0;
    InString := False;
    while Result <= Length(AText) do
    begin
      if InString then
      begin
        if AText[Result] = '\' then
          Inc(Result)
        else if AText[Result] = '"' then
        begin
          InString := False;
          if Depth = 0 then
            Exit(Result + 1);
        end;
      end
      else
        case AText[Result] of
          '"':
            InString := True;
          '{', '[':
            Inc(Depth);
          '}', ']':
            begin
              Dec(Depth);
              if Depth = 0 then
                Exit(Result + 1);
            end;
        end;
      Inc(Result);
    end;
    Exit;
  end;
  while (Result <= Length(AText)) and
        not CharInSet(AText[Result], [' ', #9, #10, #13, '{', '[', '"']) do
    Inc(Result);
end;

{ in.json holds one JSON value per document, one after another; as one JSON
  array of them }
function JSONStreamAsArray(const AText: string): string;
var
  Buffer: TStringBuffer;
  Count, Index, ValueEnd: Integer;
begin
  Buffer := TStringBuffer.Create;
  Buffer.AppendChar('[');
  Count := 0;
  Index := 1;
  while True do
  begin
    while (Index <= Length(AText)) and
          CharInSet(AText[Index], [' ', #9, #10, #13]) do
      Inc(Index);
    if Index > Length(AText) then
      Break;
    ValueEnd := JSONValueEnd(AText, Index);
    if Count > 0 then
      Buffer.AppendChar(',');
    Buffer.Append(Copy(AText, Index, ValueEnd - Index));
    Inc(Count);
    Index := ValueEnd;
  end;
  Buffer.AppendChar(']');
  Result := Buffer.ToString;
end;

function DescribeNode(const ANode: TGocciaDataNode): string;
begin
  case ANode.Kind of
    dnkNull:
      Result := 'null';
    dnkBoolean:
      Result := BoolToStr(ANode.BooleanValue, 'true', 'false');
    dnkNumber:
      Result := NumberToString(ANode.NumberValue);
    dnkString:
      Result := QuoteJSONString(ANode.StringValue);
    dnkArray:
      Result := Format('an array of %d', [ANode.Count]);
    dnkObject:
      Result := Format('an object of %d keys', [Length(ANode.OwnKeys)]);
  else
    Result := 'a tagged value';
  end;
end;

{ JSON value equality: objects compare their keys as a set, as JSON objects
  do }
function SameJSONValue(const AExpected, AActual: TGocciaDataNode;
  const APath: string; out AMessage: string): Boolean;
var
  I: Integer;
  Keys: TArray<string>;
  ChildPath: string;
begin
  if AExpected.Kind <> AActual.Kind then
  begin
    AMessage := Format('%s: expected %s, got %s',
      [APath, DescribeNode(AExpected), DescribeNode(AActual)]);
    Exit(False);
  end;
  case AExpected.Kind of
    dnkArray:
      begin
        if AExpected.Count <> AActual.Count then
        begin
          AMessage := Format('%s: expected %s, got %s',
            [APath, DescribeNode(AExpected), DescribeNode(AActual)]);
          Exit(False);
        end;
        for I := 0 to AExpected.Count - 1 do
          if not SameJSONValue(AExpected.Items[I], AActual.Items[I],
              Format('%s[%d]', [APath, I]), AMessage) then
            Exit(False);
        Exit(True);
      end;
    dnkObject:
      begin
        Keys := AExpected.OwnKeys;
        if Length(Keys) <> Length(AActual.OwnKeys) then
        begin
          AMessage := Format('%s: expected %s, got %s',
            [APath, DescribeNode(AExpected), DescribeNode(AActual)]);
          Exit(False);
        end;
        for I := 0 to High(Keys) do
        begin
          ChildPath := APath + '[' + QuoteJSONString(Keys[I]) + ']';
          if not AActual.HasProperty(Keys[I]) then
          begin
            AMessage := ChildPath + ': missing';
            Exit(False);
          end;
          if not SameJSONValue(AExpected.GetProperty(Keys[I]),
              AActual.GetProperty(Keys[I]), ChildPath, AMessage) then
            Exit(False);
        end;
        Exit(True);
      end;
    dnkBoolean:
      Result := AExpected.BooleanValue = AActual.BooleanValue;
    dnkNumber:
      Result := AExpected.NumberValue = AActual.NumberValue;
    dnkString:
      Result := AExpected.StringValue = AActual.StringValue;
  else
    Result := True;
  end;
  if not Result then
    AMessage := Format('%s: expected %s, got %s',
      [APath, DescribeNode(AExpected), DescribeNode(AActual)]);
end;

constructor TYAMLComplianceExecutor.Create(const AExecutable,
  ASuiteDirectory: string; const ATimeoutMilliseconds: Integer);
begin
  inherited Create;
  FExecutable := AExecutable;
  FSuiteDirectory := ASuiteDirectory;
  FTimeoutMilliseconds := ATimeoutMilliseconds;
end;

procedure TYAMLComplianceExecutor.ExecuteCase(const ACaseID: string;
  const AIndex: Integer; out AResult: TComplianceCaseResult);
var
  ActualJSON, ExpectedJSON: TGocciaDataTree;
  Arguments: TStringList;
  Directory, MessageText, ValuesPath: string;
  ProcessResult: TComplianceProcessResult;
begin
  Directory := CaseDirectory(FSuiteDirectory, ACaseID);
  AResult.ID := ACaseID;
  AResult.Valid := not FileExists(CaseFile(Directory, CASE_ERROR_FILE));
  AResult.Outcome := coInfrastructure;
  Arguments := TStringList.Create;
  try
    Arguments.Add('--worker');
    Arguments.Add('--case=' + Directory);
    ProcessResult := RunComplianceProcess(FExecutable, Arguments,
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
  case ProcessResult.ExitCode of
    WORKER_EXIT_PARSED, WORKER_EXIT_NO_JSON_FORM:
      ;
    WORKER_EXIT_REJECTED:
      begin
        if AResult.Valid then
          AResult.Outcome := coFalseReject
        else
        begin
          AResult.Outcome := coPassed;
          AResult.MessageText := '';
        end;
        Exit;
      end;
    WORKER_EXIT_USAGE:
      Exit;
  else
    AResult.Outcome := coCrash;
    Exit;
  end;
  if not AResult.Valid then
  begin
    AResult.Outcome := coFalseAccept;
    AResult.MessageText := '';
    Exit;
  end;
  ValuesPath := CaseFile(Directory, CASE_VALUES_FILE);
  if not FileExists(ValuesPath) then
  begin
    AResult.Outcome := coPassed;
    AResult.MessageText := '';
    Exit;
  end;
  if ProcessResult.ExitCode = WORKER_EXIT_NO_JSON_FORM then
  begin
    AResult.Outcome := coMismatch;
    Exit;
  end;
  ExpectedJSON := nil;
  ActualJSON := nil;
  try
    try
      ExpectedJSON := ParseJSONDataTree(
        JSONStreamAsArray(ReadUTF8FileText(ValuesPath)));
      ActualJSON := ParseJSONDataTree(Trim(ProcessResult.StdOutText));
      if SameJSONValue(ExpectedJSON.Root, ActualJSON.Root, 'documents',
          MessageText) then
      begin
        AResult.Outcome := coPassed;
        AResult.MessageText := '';
      end
      else
      begin
        AResult.Outcome := coMismatch;
        AResult.MessageText := MessageText;
      end;
    finally
      ActualJSON.Free;
      ExpectedJSON.Free;
    end;
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then
        raise;
      AResult.Outcome := coInfrastructure;
      AResult.MessageText := E.Message;
    end;
  end;
end;

function IsIndexDirectory(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := Low(INDEX_DIRECTORIES) to High(INDEX_DIRECTORIES) do
    if AName = INDEX_DIRECTORIES[I] then
      Exit(True);
  Result := False;
end;

{ The names of the directories directly in ADirectory, sorted }
function SubdirectoryNames(const ADirectory: string): TStringList;
var
  Entry: TSearchRec;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + AllFilesMask,
      faDirectory, Entry) = 0 then
    try
      repeat
        if ((Entry.Attr and faDirectory) <> 0) and
           (Entry.Name <> '.') and (Entry.Name <> '..') and
           not StartsStr('.', Entry.Name) then
          Result.Add(Entry.Name);
      until FindNext(Entry) <> 0;
    finally
      FindClose(Entry);
    end;
end;

{ A case is a directory with an in.yaml: a top-level one, or one level down
  for a case the suite splits into numbered variants (2G84/00) }
function DiscoverCases(const ASuiteDirectory: string): TStringList;
var
  I, J: Integer;
  Names, Variants: TStringList;
  Directory: string;
begin
  Result := TStringList.Create;
  Names := SubdirectoryNames(ASuiteDirectory);
  try
    for I := 0 to Names.Count - 1 do
    begin
      if IsIndexDirectory(Names[I]) then
        Continue;
      Directory := CaseDirectory(ASuiteDirectory, Names[I]);
      if FileExists(CaseFile(Directory, CASE_INPUT_FILE)) then
      begin
        Result.Add(Names[I]);
        Continue;
      end;
      Variants := SubdirectoryNames(Directory);
      try
        for J := 0 to Variants.Count - 1 do
          if FileExists(CaseFile(CaseDirectory(Directory, Variants[J]),
              CASE_INPUT_FILE)) then
            Result.Add(Names[I] + '/' + Variants[J]);
      finally
        Variants.Free;
      end;
    end;
  finally
    Names.Free;
  end;
end;

{ The case IDs the known-failures file lists, one per line; text after "#"
  is a comment }
function ReadKnownFailures(const APath: string): TStringList;
var
  I, CommentAt: Integer;
  Lines: TStringList;
  LineText: string;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupError;
  if not FileExists(APath) then
    raise Exception.CreateFmt('Known-failures file not found: %s', [APath]);
  Lines := TStringList.Create;
  try
    Lines.Text := ReadUTF8FileText(APath);
    for I := 0 to Lines.Count - 1 do
    begin
      LineText := Lines[I];
      CommentAt := Pos('#', LineText);
      if CommentAt > 0 then
        LineText := Copy(LineText, 1, CommentAt - 1);
      LineText := Trim(LineText);
      if LineText <> '' then
        Result.Add(LineText);
    end;
  finally
    Lines.Free;
  end;
end;

function JSONStringArray(const AValues: TStrings): string;
var
  Buffer: TStringBuffer;
  I: Integer;
begin
  Buffer := TStringBuffer.Create;
  Buffer.AppendChar('[');
  for I := 0 to AValues.Count - 1 do
  begin
    if I > 0 then
      Buffer.AppendChar(',');
    Buffer.Append(QuoteJSONString(AValues[I]));
  end;
  Buffer.AppendChar(']');
  Result := Buffer.ToString;
end;

procedure PrintCaseIDs(const AHeading: string; const AIDs: TStrings);
var
  I: Integer;
begin
  if AIDs.Count = 0 then
    Exit;
  WriteLn(AHeading, ' (', AIDs.Count, '):');
  for I := 0 to AIDs.Count - 1 do
    WriteLn('  ', AIDs[I]);
end;

procedure RunWorker;
var
  Directory: string;
  Parser: TGocciaYAMLParser;
  Documents: TGocciaDataTree;
begin
  if not ReadOptionValue('case', Directory) then
    Halt(WORKER_EXIT_USAGE);
  Parser := TGocciaYAMLParser.Create;
  try
    try
      Documents := Parser.ParseDocuments(
        ReadUTF8FileText(CaseFile(Directory, CASE_INPUT_FILE)));
      try
        WriteLn(PlainJSON(Documents.Root));
      finally
        Documents.Free;
      end;
    except
      on E: EYAMLNoJSONForm do
      begin
        WriteLn(E.Message);
        Halt(WORKER_EXIT_NO_JSON_FORM);
      end;
      { the errors YAML.parse reports for text it cannot read }
      on E: EGocciaYAMLParseError do
      begin
        WriteLn(E.Message);
        Halt(WORKER_EXIT_REJECTED);
      end;
      on E: EGocciaDataDepthError do
      begin
        WriteLn(E.Message);
        Halt(WORKER_EXIT_REJECTED);
      end;
      { fault-guard: a case's worker process, whose status WORKER_EXIT_UNEXPECTED_EXCEPTION the coordinator
        records as an infrastructure result naming the exception's class }
      on E: Exception do
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        Halt(WORKER_EXIT_UNEXPECTED_EXCEPTION);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

{ to stdout when asked for with --help, otherwise to stderr }
procedure PrintUsage(var AOutput: Text);
begin
  WriteLn(AOutput, 'Usage: GocciaYAMLComplianceRunner --suite-dir=PATH [options]');
  WriteLn(AOutput, '  --pin-file=PATH        Expected suite revision pin');
  WriteLn(AOutput, '  --known-failures=PATH  Cases allowed to fail');
  WriteLn(AOutput, '  --jobs=N               Maximum concurrent case processes');
  WriteLn(AOutput, '  --timeout=N            Per-case timeout in milliseconds');
  WriteLn(AOutput, '  --output=PATH          Write the complete JSON report');
end;

function RunCoordinator: Integer;
var
  Cases, KnownFailures, NowPassing, Unexpected, Unknown: TStringList;
  Coordinator: TComplianceCoordinator;
  Executor: TYAMLComplianceExecutor;
  I, Index, Jobs, TimeoutMilliseconds: Integer;
  KnownFailuresPath, OutputPath, PinPath, Revision, SuiteDirectory: string;
  ReportJSON: string;
  Results: TComplianceCaseResultArray;
  DurationMilliseconds, StartedAt: QWord;
  Summary: TComplianceSummary;
begin
  if not ReadOptionValue('suite-dir', SuiteDirectory) then
  begin
    PrintUsage(StdErr);
    Exit(2);
  end;
  SuiteDirectory := ExpandFileName(SuiteDirectory);
  if not ReadOptionValue('pin-file', PinPath) then
    PinPath := DEFAULT_PIN_PATH;
  if not ReadOptionValue('known-failures', KnownFailuresPath) then
    KnownFailuresPath := DEFAULT_KNOWN_FAILURES_PATH;
  Revision := ReadRequiredPin(PinPath);
  VerifySuiteRevision(SuiteDirectory, Revision);
  Jobs := PositiveIntegerOption('jobs', DefaultComplianceJobs);
  TimeoutMilliseconds := PositiveIntegerOption('timeout',
    DEFAULT_COMPLIANCE_TIMEOUT_MS);
  ReadOptionValue('output', OutputPath);

  KnownFailures := nil;
  NowPassing := TStringList.Create;
  Unexpected := TStringList.Create;
  Unknown := TStringList.Create;
  Cases := DiscoverCases(SuiteDirectory);
  try
    if Cases.Count = 0 then
      raise Exception.CreateFmt('No YAML cases discovered in %s',
        [SuiteDirectory]);
    KnownFailures := ReadKnownFailures(KnownFailuresPath);
    Executor := TYAMLComplianceExecutor.Create(ExpandFileName(ParamStr(0)),
      SuiteDirectory, TimeoutMilliseconds);
    Coordinator := TComplianceCoordinator.Create(Executor);
    StartedAt := GetTickCount64;
    try
      Results := Coordinator.Run(Cases, Jobs);
    finally
      Coordinator.Free;
      Executor.Free;
    end;
    DurationMilliseconds := GetTickCount64 - StartedAt;
    Summary := SummarizeComplianceResults(Results);

    for I := 0 to High(Results) do
    begin
      if KnownFailures.Find(Results[I].ID, Index) then
      begin
        if Results[I].Outcome = coPassed then
          NowPassing.Add(Results[I].ID);
      end
      else if Results[I].Outcome <> coPassed then
        Unexpected.Add(Results[I].ID);
    end;
    for I := 0 to KnownFailures.Count - 1 do
      if Cases.IndexOf(KnownFailures[I]) < 0 then
        Unknown.Add(KnownFailures[I]);

    ReportJSON := Format(
      '{"reportVersion":%d,"runner":{"name":%s,"version":%d},' +
      '"suite":{"name":%s,"version":%s,"revision":%s},' +
      '"mode":"native-values","jobs":%d,' +
      '"timing":{"durationMilliseconds":%d,"timeoutMilliseconds":%d},' +
      '"summary":%s,' +
      '"knownFailures":{"path":%s,"listed":%d,"unexpectedFailures":%s,' +
      '"nowPassing":%s,"notInSuite":%s},' +
      '"cases":%s}',
      [COMPLIANCE_REPORT_VERSION,
       QuoteJSONString('GocciaYAMLComplianceRunner'),
       COMPLIANCE_RUNNER_VERSION, QuoteJSONString(SUITE_NAME),
       QuoteJSONString(SUITE_VERSION), QuoteJSONString(Revision),
       Min(Jobs, Max(1, Summary.Total)), DurationMilliseconds,
       TimeoutMilliseconds, ComplianceSummaryJSON(Summary),
       QuoteJSONString(KnownFailuresPath), KnownFailures.Count,
       JSONStringArray(Unexpected), JSONStringArray(NowPassing),
       JSONStringArray(Unknown), ComplianceCasesJSON(Results)]);
    if OutputPath <> '' then
      WriteJSONFile(OutputPath, ReportJSON);
    WriteLn('YAML compliance: ', Summary.Passed, '/', Summary.Total,
      ' passed, ', KnownFailures.Count, ' known failures listed');
    WriteLn(ComplianceSummaryJSON(Summary));
    PrintCaseIDs('Failing cases not in ' + KnownFailuresPath, Unexpected);
    PrintCaseIDs('Passing cases to remove from ' + KnownFailuresPath,
      NowPassing);
    PrintCaseIDs('Cases in ' + KnownFailuresPath + ' that the suite lacks',
      Unknown);
    if (Unexpected.Count > 0) or (NowPassing.Count > 0) or
       (Unknown.Count > 0) then
      Result := 1
    else
      Result := 0;
  finally
    Cases.Free;
    KnownFailures.Free;
    NowPassing.Free;
    Unexpected.Free;
    Unknown.Free;
  end;
end;

begin
  if HasCommandLineFlag('worker') then
    RunWorker
  else if HasCommandLineFlag('help') then
  begin
    PrintUsage(Output);
    ExitCode := 0;
  end
  else
  begin
    try
      {$IFDEF GOCCIA_FAULT_INJECTION}
      InjectFaultAt('startup');
      {$ENDIF}
      ExitCode := RunCoordinator;
    except
      on E: Exception do
      begin
        { each case runs in a worker process of its own; a fault in the coordinator itself ends the run (ADR 0201) }
        if IsEngineFault(E) then
          AbortOnEngineFault('GocciaYAMLComplianceRunner', E);
        WriteLn(StdErr, 'Error: ', E.Message);
        ExitCode := 2;
      end;
    end;
  end;
end.
