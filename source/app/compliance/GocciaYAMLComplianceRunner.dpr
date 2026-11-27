program GocciaYAMLComplianceRunner;

{ Runs the YAML parser over a prepared checkout of the yaml-test-suite data
  branch (github.com/yaml/yaml-test-suite). Each case runs in a worker process
  of this executable. A valid case passes when TGocciaYAMLEventParser gives
  the events of its test.event and, where the case has in.json, when
  TGocciaYAMLParser's documents equal those JSON values; a case with an error
  file passes when the event parser rejects it. Any other result fails the
  run. }

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
  Goccia.JSON.Utils,
  Goccia.TextFiles,
  Goccia.YAML,
  Goccia.YAML.Events,

  NumericText,
  StringBuffer;

const
  SUITE_NAME = 'yaml-test-suite';
  SUITE_VERSION = '1.2.2';
  DEFAULT_PIN_PATH = 'tests/compliance/yaml-test-suite.pin';
  CASE_INPUT_FILE = 'in.yaml';
  CASE_VALUES_FILE = 'in.json';
  CASE_EVENTS_FILE = 'test.event';
  CASE_ERROR_FILE = 'error';
  { the data branch's indexes of the cases by name and by tag: symlinks to
    the case directories, so walking them runs each case again }
  INDEX_DIRECTORIES: array[0..1] of string = ('name', 'tags');

  WORKER_EXIT_PARSED = 0;
  WORKER_EXIT_REJECTED = 1;
  WORKER_EXIT_USAGE = 2;
  WORKER_EXIT_UNEXPECTED_EXCEPTION = 3;
  WORKER_EXIT_NO_JSON_FORM = 4;
  { the events parsed, but composing the documents failed }
  WORKER_EXIT_VALUES_REJECTED = 5;

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

{ the worker's events, a JSON array of test.event lines, against the
  case's test.event }
function SameEvents(const AExpectedText, AActualJSON: string;
  out AMessage: string): Boolean;
var
  Actual: TGocciaDataTree;
  Expected: TStringList;
  I: Integer;
  ActualLine, ExpectedLine: string;
begin
  Expected := TStringList.Create;
  Actual := ParseJSONDataTree(AActualJSON);
  try
    Expected.Text := AExpectedText;
    for I := 0 to Max(Expected.Count, Actual.Root.Count) - 1 do
    begin
      if I < Expected.Count then
        ExpectedLine := Expected[I]
      else
        ExpectedLine := '(none)';
      if I < Actual.Root.Count then
        ActualLine := Actual.Root.Items[I].StringValue
      else
        ActualLine := '(none)';
      if ExpectedLine <> ActualLine then
      begin
        AMessage := Format('event %d: expected %s, got %s',
          [I + 1, ExpectedLine, ActualLine]);
        Exit(False);
      end;
    end;
    Result := True;
  finally
    Actual.Free;
    Expected.Free;
  end;
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
  Arguments, OutputLines: TStringList;
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
    { an error case is invalid syntax, which the event parser must find }
    WORKER_EXIT_VALUES_REJECTED:
      begin
        if AResult.Valid then
          AResult.Outcome := coFalseReject
        else
          AResult.Outcome := coFalseAccept;
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

  ExpectedJSON := nil;
  ActualJSON := nil;
  OutputLines := TStringList.Create;
  try
    try
      OutputLines.Text := ProcessResult.StdOutText;
      if OutputLines.Count < 2 then
        raise Exception.Create('the worker printed no events and documents');
      if not SameEvents(
          ReadUTF8FileText(CaseFile(Directory, CASE_EVENTS_FILE)),
          OutputLines[0], MessageText) then
      begin
        AResult.Outcome := coMismatch;
        AResult.MessageText := MessageText;
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
        AResult.MessageText := OutputLines[1];
        Exit;
      end;
      ExpectedJSON := ParseJSONDataTree(
        JSONStreamAsArray(ReadUTF8FileText(ValuesPath)));
      ActualJSON := ParseJSONDataTree(OutputLines[1]);
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
      OutputLines.Free;
      ActualJSON.Free;
      ExpectedJSON.Free;
    end;
  except
    on E: Exception do
    begin
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

procedure PrintFailingCases(const AResults: TComplianceCaseResultArray);
var
  I: Integer;
begin
  for I := 0 to High(AResults) do
    if AResults[I].Outcome <> coPassed then
      WriteLn('  ', AResults[I].ID, ' ', ComplianceOutcomeName(AResults[I].Outcome),
        ': ', AResults[I].MessageText);
end;

{ the stream's events as a JSON array of test.event lines }
function EventsJSON(const AText: string): string;
var
  Buffer: TStringBuffer;
  Event: TGocciaYAMLEvent;
  Events: TGocciaYAMLEventParser;
begin
  Buffer := TStringBuffer.Create;
  Buffer.AppendChar('[');
  Events := TGocciaYAMLEventParser.Create(AText);
  try
    repeat
      Event := Events.NextEvent;
      if Buffer.Length > 1 then
        Buffer.AppendChar(',');
      AppendASCIIJSONString(Buffer, YAMLEventText(Event));
    until Event.Kind = yekStreamEnd;
  finally
    Events.Free;
  end;
  Buffer.AppendChar(']');
  Result := Buffer.ToString;
end;

{ prints the events on one line, then the documents as plain JSON }
procedure RunWorker;
var
  Directory, Events, SourceText: string;
  Parser: TGocciaYAMLParser;
  Documents: TGocciaDataTree;
begin
  if not ReadOptionValue('case', Directory) then
    Halt(WORKER_EXIT_USAGE);
  try
    SourceText := ReadUTF8FileText(CaseFile(Directory, CASE_INPUT_FILE));
    Events := EventsJSON(SourceText);
  except
    on E: EGocciaYAMLParseError do
    begin
      WriteLn(E.Message);
      Halt(WORKER_EXIT_REJECTED);
    end;
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Halt(WORKER_EXIT_UNEXPECTED_EXCEPTION);
    end;
  end;

  Parser := TGocciaYAMLParser.Create;
  try
    try
      Documents := Parser.ParseDocuments(SourceText);
      try
        WriteLn(Events);
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
        Halt(WORKER_EXIT_VALUES_REJECTED);
      end;
      on E: EGocciaDataDepthError do
      begin
        WriteLn(E.Message);
        Halt(WORKER_EXIT_VALUES_REJECTED);
      end;
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
  WriteLn(AOutput, '  --pin-file=PATH   Expected suite revision pin');
  WriteLn(AOutput, '  --jobs=N          Maximum concurrent case processes');
  WriteLn(AOutput, '  --timeout=N       Per-case timeout in milliseconds');
  WriteLn(AOutput, '  --output=PATH     Write the complete JSON report');
end;

function RunCoordinator: Integer;
var
  Cases: TStringList;
  Coordinator: TComplianceCoordinator;
  Executor: TYAMLComplianceExecutor;
  Jobs, TimeoutMilliseconds: Integer;
  OutputPath, PinPath, Revision, SuiteDirectory: string;
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
  Revision := ReadRequiredPin(PinPath);
  VerifySuiteRevision(SuiteDirectory, Revision);
  Jobs := PositiveIntegerOption('jobs', DefaultComplianceJobs);
  TimeoutMilliseconds := PositiveIntegerOption('timeout',
    DEFAULT_COMPLIANCE_TIMEOUT_MS);
  ReadOptionValue('output', OutputPath);

  Cases := DiscoverCases(SuiteDirectory);
  try
    if Cases.Count = 0 then
      raise Exception.CreateFmt('No YAML cases discovered in %s',
        [SuiteDirectory]);
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
  finally
    Cases.Free;
  end;
  DurationMilliseconds := GetTickCount64 - StartedAt;
  Summary := SummarizeComplianceResults(Results);

  ReportJSON := Format(
    '{"reportVersion":%d,"runner":{"name":%s,"version":%d},' +
    '"suite":{"name":%s,"version":%s,"revision":%s},' +
    '"mode":"native-events-and-values","jobs":%d,' +
    '"timing":{"durationMilliseconds":%d,"timeoutMilliseconds":%d},' +
    '"summary":%s,"cases":%s}',
    [COMPLIANCE_REPORT_VERSION,
     QuoteJSONString('GocciaYAMLComplianceRunner'),
     COMPLIANCE_RUNNER_VERSION, QuoteJSONString(SUITE_NAME),
     QuoteJSONString(SUITE_VERSION), QuoteJSONString(Revision),
     Min(Jobs, Max(1, Summary.Total)), DurationMilliseconds,
     TimeoutMilliseconds, ComplianceSummaryJSON(Summary),
     ComplianceCasesJSON(Results)]);
  if OutputPath <> '' then
    WriteJSONFile(OutputPath, ReportJSON);
  WriteLn('YAML compliance: ', Summary.Passed, '/', Summary.Total, ' passed');
  WriteLn(ComplianceSummaryJSON(Summary));
  PrintFailingCases(Results);
  if Summary.Failed > 0 then
    Result := 1
  else
    Result := 0;
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
      ExitCode := RunCoordinator;
    except
      on E: Exception do
      begin
        WriteLn(StdErr, 'Error: ', E.Message);
        ExitCode := 2;
      end;
    end;
  end;
end.
