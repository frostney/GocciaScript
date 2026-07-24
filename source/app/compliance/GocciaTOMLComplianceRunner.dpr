program GocciaTOMLComplianceRunner;

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
  Goccia.Threading,
  Goccia.TOML,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,

  FileUtils,
  StringBuffer;

const
  SUITE_NAME = 'toml-test';
  SUITE_VERSION = '1.1.0';
  DEFAULT_PIN_PATH = 'tests/compliance/toml-test.pin';
  SUITE_FILE_LIST = 'tests/files-toml-1.1.0';

type
  TTOMLComplianceExecutor = class(TComplianceCaseExecutor)
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

function ScalarKindName(const AKind: TGocciaTOMLScalarKind): string;
begin
  case AKind of
    tskString:
      Result := 'string';
    tskInteger:
      Result := 'integer';
    tskFloat:
      Result := 'float';
    tskBool:
      Result := 'bool';
    tskDateTime:
      Result := 'datetime';
    tskDateTimeLocal:
      Result := 'datetime-local';
    tskDateLocal:
      Result := 'date-local';
    tskTimeLocal:
      Result := 'time-local';
  else
    Result := 'string';
  end;
end;

function SerializeNode(const ANode: TGocciaTOMLNode): string;
var
  Buffer: TStringBuffer;
  I: Integer;
  Pair: TGocciaTOMLNodeMap.TKeyValuePair;
begin
  case ANode.Kind of
    tnkScalar:
      Result := '{"type":' + QuoteJSONString(ScalarKindName(ANode.ScalarKind)) +
        ',"value":' + QuoteJSONString(ANode.CanonicalValue) + '}';
    tnkArray,
    tnkArrayOfTables:
      begin
        Buffer := TStringBuffer.Create;
        Buffer.AppendChar('[');
        for I := 0 to ANode.Items.Count - 1 do
        begin
          if I > 0 then
            Buffer.AppendChar(',');
          Buffer.Append(SerializeNode(ANode.Items[I]));
        end;
        Buffer.AppendChar(']');
        Result := Buffer.ToString;
      end;
    tnkTable:
      begin
        Buffer := TStringBuffer.Create;
        Buffer.AppendChar('{');
        I := 0;
        for Pair in ANode.Children do
        begin
          if I > 0 then
            Buffer.AppendChar(',');
          Buffer.Append(QuoteJSONString(Pair.Key));
          Buffer.AppendChar(':');
          Buffer.Append(SerializeNode(Pair.Value));
          Inc(I);
        end;
        Buffer.AppendChar('}');
        Result := Buffer.ToString;
      end;
  else
    Result := 'null';
  end;
end;

function NormalizeDateTimeText(const AKind, AValue: string): string;
var
  DotIndex, EndIndex: Integer;
begin
  Result := StringReplace(AValue, ' ', 'T', []);
  Result := StringReplace(Result, 't', 'T', []);
  Result := StringReplace(Result, 'z', 'Z', []);
  if EndsText('Z', Result) then
    Result := Copy(Result, 1, Length(Result) - 1) + '+00:00';
  if AKind = 'date-local' then
    Exit;
  DotIndex := Pos('.', Result);
  if DotIndex = 0 then
    Exit;
  EndIndex := DotIndex + 1;
  while (EndIndex <= Length(Result)) and CharInSet(Result[EndIndex], ['0'..'9']) do
    Inc(EndIndex);
  while (EndIndex > DotIndex + 1) and (Result[EndIndex - 1] = '0') do
  begin
    Delete(Result, EndIndex - 1, 1);
    Dec(EndIndex);
  end;
  if EndIndex = DotIndex + 1 then
    Delete(Result, DotIndex, 1);
end;

function SameDoubleBits(const ALeft, ARight: Double): Boolean;
var
  LeftValue, RightValue: Double;
  LeftBits: Int64 absolute LeftValue;
  RightBits: Int64 absolute RightValue;
begin
  LeftValue := ALeft;
  RightValue := ARight;
  Result := LeftBits = RightBits;
end;

function WithoutSign(const AValue: string): string;
begin
  Result := AValue;
  if (Result <> '') and CharInSet(Result[1], ['+', '-']) then
    Delete(Result, 1, 1);
end;

function CompareTaggedJSON(const AExpected, AActual: TGocciaValue;
  const APath: string; out AMessage: string): Boolean;
var
  ExpectedObject, ActualObject: TGocciaObjectValue;
  ExpectedArray, ActualArray: TGocciaArrayValue;
  ExpectedType, ActualType, ExpectedValue, ActualValue, ChildPath: string;
  ExpectedFloat, ActualFloat: Double;
  FloatFormat: TFormatSettings;
  I: Integer;
  ExpectedKeys: TArray<string>;
begin
  if ((AExpected is TGocciaObjectValue) <>
      (AActual is TGocciaObjectValue)) or
     ((AExpected is TGocciaArrayValue) <>
      (AActual is TGocciaArrayValue)) then
  begin
    AMessage := APath + ': JSON kinds differ';
    Exit(False);
  end;
  if AExpected is TGocciaArrayValue then
  begin
    ExpectedArray := TGocciaArrayValue(AExpected);
    ActualArray := TGocciaArrayValue(AActual);
    if ExpectedArray.Elements.Count <> ActualArray.Elements.Count then
    begin
      AMessage := APath + ': array lengths differ';
      Exit(False);
    end;
    for I := 0 to ExpectedArray.Elements.Count - 1 do
      if not CompareTaggedJSON(ExpectedArray.Elements[I],
          ActualArray.Elements[I], Format('%s[%d]', [APath, I]),
          AMessage) then
        Exit(False);
    Exit(True);
  end;
  if AExpected is TGocciaObjectValue then
  begin
    ExpectedObject := TGocciaObjectValue(AExpected);
    ActualObject := TGocciaObjectValue(AActual);
    ExpectedKeys := ExpectedObject.GetOwnPropertyKeys;
    if Length(ExpectedKeys) <> Length(ActualObject.GetOwnPropertyKeys) then
    begin
      AMessage := APath + ': object keys differ';
      Exit(False);
    end;
    if ExpectedObject.HasOwnProperty('type') and
       ExpectedObject.HasOwnProperty('value') and
       (Length(ExpectedKeys) = 2) then
    begin
      if not ActualObject.HasOwnProperty('type') or
         not ActualObject.HasOwnProperty('value') then
      begin
        AMessage := APath + ': malformed tagged value';
        Exit(False);
      end;
      ExpectedType := ExpectedObject.GetProperty('type').ToStringLiteral.Value;
      ActualType := ActualObject.GetProperty('type').ToStringLiteral.Value;
      if ExpectedType <> ActualType then
      begin
        AMessage := Format('%s: expected type %s, got %s',
          [APath, ExpectedType, ActualType]);
        Exit(False);
      end;
      ExpectedValue :=
        ExpectedObject.GetProperty('value').ToStringLiteral.Value;
      ActualValue := ActualObject.GetProperty('value').ToStringLiteral.Value;
      if ExpectedType = 'float' then
      begin
        if SameText(WithoutSign(ExpectedValue), 'nan') or
           SameText(WithoutSign(ActualValue), 'nan') then
        begin
          Result := SameText(WithoutSign(ExpectedValue),
            WithoutSign(ActualValue));
          if not Result then
            AMessage := Format('%s: expected float %s, got %s',
              [APath, ExpectedValue, ActualValue]);
          Exit;
        end;
        FloatFormat := DefaultFormatSettings;
        FloatFormat.DecimalSeparator := '.';
        Result := TryStrToFloat(ExpectedValue, ExpectedFloat, FloatFormat) and
          TryStrToFloat(ActualValue, ActualFloat, FloatFormat) and
          SameDoubleBits(ExpectedFloat, ActualFloat);
        if not Result then
          AMessage := Format('%s: expected float %s, got %s',
            [APath, ExpectedValue, ActualValue]);
        Exit;
      end;
      if AnsiMatchText(ExpectedType, ['datetime', 'datetime-local',
          'date-local', 'time-local']) then
        Result := NormalizeDateTimeText(ExpectedType, ExpectedValue) =
          NormalizeDateTimeText(ActualType, ActualValue)
      else if ExpectedType = 'bool' then
        Result := SameText(ExpectedValue, ActualValue)
      else
        Result := ExpectedValue = ActualValue;
      if not Result then
        AMessage := Format('%s: expected %s, got %s',
          [APath, ExpectedValue, ActualValue]);
      Exit;
    end;
    for I := 0 to High(ExpectedKeys) do
    begin
      if not ActualObject.HasOwnProperty(ExpectedKeys[I]) then
      begin
        AMessage := APath + ': object keys differ';
        Exit(False);
      end;
      if APath = '<root>' then
        ChildPath := ExpectedKeys[I]
      else
        ChildPath := APath + '.' + ExpectedKeys[I];
      if not CompareTaggedJSON(ExpectedObject.GetProperty(ExpectedKeys[I]),
          ActualObject.GetProperty(ExpectedKeys[I]), ChildPath, AMessage) then
        Exit(False);
    end;
    Exit(True);
  end;
  Result := AExpected.ToStringLiteral.Value =
    AActual.ToStringLiteral.Value;
  if not Result then
    AMessage := APath + ': values differ';
end;

constructor TTOMLComplianceExecutor.Create(const AExecutable,
  ASuiteDirectory: string; const ATimeoutMilliseconds: Integer);
begin
  inherited Create;
  FExecutable := AExecutable;
  FSuiteDirectory := ASuiteDirectory;
  FTimeoutMilliseconds := ATimeoutMilliseconds;
end;

procedure TTOMLComplianceExecutor.ExecuteCase(const ACaseID: string;
  const AIndex: Integer; out AResult: TComplianceCaseResult);
var
  ActualJSON, ExpectedJSON: TGocciaValue;
  JSONParser: TGocciaJSONParser;
  Arguments: TStringList;
  CasePath, ExpectedPath, MessageText: string;
  ProcessResult: TComplianceProcessResult;
begin
  AResult.ID := ACaseID;
  AResult.Valid := StartsText('valid/', ACaseID);
  AResult.Outcome := coInfrastructure;
  CasePath := IncludeTrailingPathDelimiter(FSuiteDirectory) + 'tests' +
    DirectorySeparator + StringReplace(ACaseID, '/', DirectorySeparator,
      [rfReplaceAll]);
  Arguments := TStringList.Create;
  try
    Arguments.Add('--worker');
    Arguments.Add('--case=' + CasePath);
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
  if AResult.Valid then
  begin
    if ProcessResult.ExitCode = 1 then
    begin
      AResult.Outcome := coFalseReject;
      Exit;
    end;
    if ProcessResult.ExitCode <> 0 then
    begin
      if ProcessResult.ExitCode = 2 then
        AResult.Outcome := coInfrastructure
      else
        AResult.Outcome := coCrash;
      Exit;
    end;
    ExpectedPath := ChangeFileExt(CasePath, '.json');
    try
      JSONParser := TGocciaJSONParser.Create;
      try
        ExpectedJSON := JSONParser.Parse(ReadUTF8FileText(ExpectedPath));
        ActualJSON := JSONParser.Parse(Trim(ProcessResult.StdOutText));
        if CompareTaggedJSON(ExpectedJSON, ActualJSON, '<root>',
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
        JSONParser.Free;
      end;
    except
      on E: Exception do
      begin
        AResult.Outcome := coInfrastructure;
        AResult.MessageText := E.Message;
      end;
    end;
  end
  else if ProcessResult.ExitCode = 1 then
  begin
    AResult.Outcome := coPassed;
    AResult.MessageText := '';
  end
  else if ProcessResult.ExitCode = 0 then
    AResult.Outcome := coFalseAccept
  else if ProcessResult.ExitCode = 2 then
    AResult.Outcome := coInfrastructure
  else
    AResult.Outcome := coCrash;
end;

function DiscoverCases(const ASuiteDirectory: string): TStringList;
var
  I: Integer;
  Lines: TStringList;
  LineText, ListPath: string;
begin
  Result := TStringList.Create;
  Lines := TStringList.Create;
  try
    ListPath := IncludeTrailingPathDelimiter(ASuiteDirectory) +
      StringReplace(SUITE_FILE_LIST, '/', DirectorySeparator, [rfReplaceAll]);
    if not FileExists(ListPath) then
      raise Exception.CreateFmt('Suite file list not found: %s', [ListPath]);
    Lines.Text := ReadUTF8FileText(ListPath);
    for I := 0 to Lines.Count - 1 do
    begin
      LineText := Trim(Lines[I]);
      if (LineText <> '') and (not StartsText('#', LineText)) and
         EndsText('.toml', LineText) then
        Result.Add(LineText);
    end;
  finally
    Lines.Free;
  end;
end;

procedure RunWorker;
var
  CasePath, SourceText: string;
  Parser: TGocciaTOMLParser;
  Root: TGocciaTOMLNode;
begin
  if not ReadOptionValue('case', CasePath) then
    Halt(2);
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  Parser := TGocciaTOMLParser.Create;
  try
    try
      SourceText := ReadUTF8FileText(CasePath);
      Root := Parser.ParseDocument(SourceText);
      try
        WriteLn(SerializeNode(Root));
      finally
        Root.Free;
      end;
    except
      on E: EConvertError do
      begin
        WriteLn(E.Message);
        Halt(1);
      end;
      on E: EGocciaTOMLParseError do
      begin
        WriteLn(E.Message);
        Halt(1);
      end;
      on E: Exception do
      begin
        WriteLn(E.ClassName, ': ', E.Message);
        Halt(2);
      end;
    end;
  finally
    Parser.Free;
    TGarbageCollector.Shutdown;
  end;
end;

procedure PrintUsage;
begin
  WriteLn('Usage: GocciaTOMLComplianceRunner --suite-dir=PATH [options]');
  WriteLn('  --pin-file=PATH   Expected suite revision pin');
  WriteLn('  --jobs=N          Maximum concurrent case processes');
  WriteLn('  --timeout=N       Per-case timeout in milliseconds');
  WriteLn('  --output=PATH     Write the complete JSON report');
end;

function RunCoordinator: Integer;
var
  Cases: TStringList;
  Coordinator: TComplianceCoordinator;
  Executor: TTOMLComplianceExecutor;
  Jobs, TimeoutMilliseconds: Integer;
  OutputPath, PinPath, Revision, SuiteDirectory: string;
  ReportJSON: string;
  Results: TComplianceCaseResultArray;
  StartedAt, DurationMilliseconds: QWord;
  Summary: TComplianceSummary;
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
  Jobs := PositiveIntegerOption('jobs', DefaultComplianceJobs);
  TimeoutMilliseconds := PositiveIntegerOption('timeout',
    DEFAULT_COMPLIANCE_TIMEOUT_MS);
  ReadOptionValue('output', OutputPath);

  Cases := DiscoverCases(SuiteDirectory);
  if Cases.Count = 0 then
  begin
    Cases.Free;
    raise Exception.CreateFmt('No TOML cases discovered in %s',
      [SuiteDirectory]);
  end;
  Executor := TTOMLComplianceExecutor.Create(ExpandFileName(ParamStr(0)),
    SuiteDirectory, TimeoutMilliseconds);
  Coordinator := TComplianceCoordinator.Create(Executor);
  StartedAt := GetTickCount64;
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;
  try
    Results := Coordinator.Run(Cases, Jobs);
  finally
    TGarbageCollector.Shutdown;
    Coordinator.Free;
    Executor.Free;
    Cases.Free;
  end;
  DurationMilliseconds := GetTickCount64 - StartedAt;
  Summary := SummarizeComplianceResults(Results);

  ReportJSON := Format(
    '{"reportVersion":%d,"runner":{"name":%s,"version":%d},' +
    '"suite":{"name":%s,"version":%s,"revision":%s},' +
    '"mode":"native-tagged-ast","jobs":%d,' +
    '"timing":{"durationMilliseconds":%d,"timeoutMilliseconds":%d},' +
    '"summary":%s,"cases":%s,' +
    '"sections":{"decoder":{"mode":"tagged-ast"}}}',
    [COMPLIANCE_REPORT_VERSION,
     QuoteJSONString('GocciaTOMLComplianceRunner'),
     COMPLIANCE_RUNNER_VERSION, QuoteJSONString(SUITE_NAME),
     QuoteJSONString(SUITE_VERSION), QuoteJSONString(Revision),
     Min(Jobs, Max(1, Summary.Total)), DurationMilliseconds,
     TimeoutMilliseconds, ComplianceSummaryJSON(Summary),
     ComplianceCasesJSON(Results)]);
  if OutputPath <> '' then
    WriteJSONFile(OutputPath, ReportJSON);
  WriteLn('TOML compliance: ', Summary.Passed, '/', Summary.Total, ' passed');
  WriteLn(ComplianceSummaryJSON(Summary));
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
    PrintUsage;
    ExitCode := 0;
  end
  else
  begin
    try
      ExitCode := RunCoordinator;
    except
      on E: Exception do
      begin
        WriteLn('Error: ', E.Message);
        ExitCode := 2;
      end;
    end;
  end;
end.
