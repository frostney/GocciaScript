unit Goccia.Compliance;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Threading;

const
  COMPLIANCE_REPORT_VERSION = 1;
  COMPLIANCE_RUNNER_VERSION = 1;
  DEFAULT_COMPLIANCE_TIMEOUT_MS = 5000;

type
  TComplianceOutcome = (
    coPassed,
    coFalseAccept,
    coFalseReject,
    coMismatch,
    coTimeout,
    coCrash,
    coInfrastructure
  );

  TComplianceProcessResult = record
    Started: Boolean;
    TimedOut: Boolean;
    ExitCode: Integer;
    DurationMilliseconds: QWord;
    StdOutText: string;
    StdErrText: string;
    ErrorMessage: string;
  end;

  TComplianceCaseResult = record
    ID: string;
    Valid: Boolean;
    Outcome: TComplianceOutcome;
    DurationMilliseconds: QWord;
    MessageText: string;
  end;

  TComplianceCaseResultArray = array of TComplianceCaseResult;

  TComplianceSummary = record
    Total: Integer;
    Passed: Integer;
    Failed: Integer;
    FalseAccepts: Integer;
    FalseRejects: Integer;
    Mismatches: Integer;
    Timeouts: Integer;
    Crashes: Integer;
    InfrastructureFailures: Integer;
  end;

  TComplianceCaseExecutor = class
  public
    procedure ExecuteCase(const ACaseID: string; const AIndex: Integer;
      out AResult: TComplianceCaseResult); virtual; abstract;
  end;

  TComplianceCoordinator = class
  private
    FExecutor: TComplianceCaseExecutor;
    FResults: TComplianceCaseResultArray;
    procedure WorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
  public
    constructor Create(const AExecutor: TComplianceCaseExecutor);
    function Run(const ACases: TStringList;
      const AJobCount: Integer): TComplianceCaseResultArray;
  end;

function ComplianceOutcomeName(const AOutcome: TComplianceOutcome): string;
function SummarizeComplianceResults(
  const AResults: TComplianceCaseResultArray): TComplianceSummary;
function ComplianceSummaryJSON(const ASummary: TComplianceSummary): string;
function ComplianceCasesJSON(
  const AResults: TComplianceCaseResultArray): string;
function RunComplianceProcess(const AExecutable: string;
  const AArguments: TStrings; const ATimeoutMilliseconds: Integer;
  const ACurrentDirectory: string = ''): TComplianceProcessResult;
function ReadRequiredPin(const APinPath: string): string;
procedure VerifySuiteRevision(const ASuiteDirectory,
  AExpectedRevision: string);
function ReadOptionValue(const AName: string; out AValue: string): Boolean;
function HasCommandLineFlag(const AName: string): Boolean;
function PositiveIntegerOption(const AName: string;
  const ADefaultValue: Integer): Integer;
function DefaultComplianceJobs: Integer;
procedure WriteJSONFile(const APath, AJSON: string);
function CombinedProcessMessage(
  const AResult: TComplianceProcessResult): string;

implementation

uses
  Math,
  Process,
  StrUtils,
  SysUtils,

  FileUtils,
  Pipes,
  StringBuffer,
  TextEncoding,

  Goccia.JSON.Utils;

procedure DrainPipe(const APipe: TInputPipeStream; const ATarget: TStream);
var
  Available, Count: LongInt;
  Buffer: array[0..4095] of Byte;
begin
  repeat
    Available := APipe.NumBytesAvailable;
    if Available <= 0 then
      Exit;
    Count := Min(Available, SizeOf(Buffer));
    Count := APipe.Read(Buffer, Count);
    if Count > 0 then
      ATarget.WriteBuffer(Buffer, Count);
  until Count <= 0;
end;

function StreamAsUTF8Text(const AStream: TMemoryStream): string;
var
  Bytes: TBytes;
begin
  SetLength(Bytes, AStream.Size);
  AStream.Position := 0;
  if AStream.Size > 0 then
    AStream.ReadBuffer(Bytes[0], AStream.Size);
  Result := DecodeUTF8WithReplacement(Bytes);
end;

function RunComplianceProcess(const AExecutable: string;
  const AArguments: TStrings; const ATimeoutMilliseconds: Integer;
  const ACurrentDirectory: string): TComplianceProcessResult;
var
  I: Integer;
  ProcessRunner: TProcess;
  StartedAt: QWord;
  StdOutStream, StdErrStream: TMemoryStream;
begin
  Result.Started := False;
  Result.TimedOut := False;
  Result.ExitCode := -1;
  Result.DurationMilliseconds := 0;
  Result.StdOutText := '';
  Result.StdErrText := '';
  Result.ErrorMessage := '';

  ProcessRunner := TProcess.Create(nil);
  StdOutStream := TMemoryStream.Create;
  StdErrStream := TMemoryStream.Create;
  try
    ProcessRunner.Executable := AExecutable;
    for I := 0 to AArguments.Count - 1 do
      ProcessRunner.Parameters.Add(AArguments[I]);
    if ACurrentDirectory <> '' then
      ProcessRunner.CurrentDirectory := ACurrentDirectory;
    ProcessRunner.Options := [poUsePipes, poStderrToOutPut];
    StartedAt := GetTickCount64;
    try
      ProcessRunner.Execute;
      Result.Started := True;
      while ProcessRunner.Running do
      begin
        DrainPipe(ProcessRunner.Output, StdOutStream);
        if (ATimeoutMilliseconds > 0) and
           (GetTickCount64 - StartedAt >= QWord(ATimeoutMilliseconds)) then
        begin
          Result.TimedOut := True;
          ProcessRunner.Terminate(124);
          Break;
        end;
        Sleep(5);
      end;
      ProcessRunner.WaitOnExit;
      DrainPipe(ProcessRunner.Output, StdOutStream);
      Result.ExitCode := ProcessRunner.ExitCode;
    except
      on E: Exception do
        Result.ErrorMessage := E.Message;
    end;
    Result.DurationMilliseconds := GetTickCount64 - StartedAt;
    Result.StdOutText := StreamAsUTF8Text(StdOutStream);
    Result.StdErrText := StreamAsUTF8Text(StdErrStream);
  finally
    StdErrStream.Free;
    StdOutStream.Free;
    ProcessRunner.Free;
  end;
end;

function ComplianceOutcomeName(const AOutcome: TComplianceOutcome): string;
begin
  case AOutcome of
    coPassed:
      Result := 'passed';
    coFalseAccept:
      Result := 'false_accept';
    coFalseReject:
      Result := 'false_reject';
    coMismatch:
      Result := 'mismatch';
    coTimeout:
      Result := 'timeout';
    coCrash:
      Result := 'crash';
    coInfrastructure:
      Result := 'infrastructure';
  else
    Result := 'infrastructure';
  end;
end;

function SummarizeComplianceResults(
  const AResults: TComplianceCaseResultArray): TComplianceSummary;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Total := Length(AResults);
  for I := 0 to High(AResults) do
  begin
    case AResults[I].Outcome of
      coPassed:
        Inc(Result.Passed);
      coFalseAccept:
        Inc(Result.FalseAccepts);
      coFalseReject:
        Inc(Result.FalseRejects);
      coMismatch:
        Inc(Result.Mismatches);
      coTimeout:
        Inc(Result.Timeouts);
      coCrash:
        Inc(Result.Crashes);
      coInfrastructure:
        Inc(Result.InfrastructureFailures);
    end;
  end;
  Result.Failed := Result.Total - Result.Passed;
end;

function ComplianceSummaryJSON(
  const ASummary: TComplianceSummary): string;
begin
  Result := Format(
    '{"total":%d,"passed":%d,"failed":%d,"failureClasses":' +
    '{"falseAccepts":%d,"falseRejects":%d,"mismatches":%d,' +
    '"timeouts":%d,"crashes":%d,"infrastructure":%d}}',
    [ASummary.Total, ASummary.Passed, ASummary.Failed,
     ASummary.FalseAccepts, ASummary.FalseRejects, ASummary.Mismatches,
     ASummary.Timeouts, ASummary.Crashes, ASummary.InfrastructureFailures]);
end;

function ComplianceCasesJSON(
  const AResults: TComplianceCaseResultArray): string;
var
  I: Integer;
  Buffer: TStringBuffer;
begin
  Buffer := TStringBuffer.Create;
  Buffer.AppendChar('[');
  for I := 0 to High(AResults) do
  begin
    if I > 0 then
      Buffer.AppendChar(',');
    Buffer.Append('{"id":');
    Buffer.Append(QuoteJSONString(AResults[I].ID));
    Buffer.Append(',"valid":');
    if AResults[I].Valid then
      Buffer.Append('true')
    else
      Buffer.Append('false');
    Buffer.Append(',"ok":');
    if AResults[I].Outcome = coPassed then
      Buffer.Append('true')
    else
      Buffer.Append('false');
    Buffer.Append(',"outcome":');
    Buffer.Append(QuoteJSONString(
      ComplianceOutcomeName(AResults[I].Outcome)));
    Buffer.Append(',"durationMilliseconds":');
    Buffer.Append(IntToStr(AResults[I].DurationMilliseconds));
    Buffer.Append(',"message":');
    Buffer.Append(QuoteJSONString(AResults[I].MessageText));
    Buffer.AppendChar('}');
  end;
  Buffer.AppendChar(']');
  Result := Buffer.ToString;
end;

function ReadRequiredPin(const APinPath: string): string;
var
  I: Integer;
begin
  if not FileExists(APinPath) then
    raise Exception.CreateFmt('Pin file not found: %s', [APinPath]);
  Result := Trim(ReadUTF8FileText(APinPath));
  if (Length(Result) <> 40) then
    raise Exception.CreateFmt('Pin file must contain one 40-character SHA: %s',
      [APinPath]);
  for I := 1 to Length(Result) do
    if not CharInSet(Result[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      raise Exception.CreateFmt(
        'Pin file must contain one hexadecimal SHA: %s', [APinPath]);
end;

procedure VerifySuiteRevision(const ASuiteDirectory,
  AExpectedRevision: string);
var
  Arguments: TStringList;
  ProcessResult: TComplianceProcessResult;
  ActualRevision: string;
begin
  if not DirectoryExists(ASuiteDirectory) then
    raise Exception.CreateFmt('Suite directory not found: %s',
      [ASuiteDirectory]);
  Arguments := TStringList.Create;
  try
    Arguments.Add('-C');
    Arguments.Add(ASuiteDirectory);
    Arguments.Add('rev-parse');
    Arguments.Add('HEAD');
    ProcessResult := RunComplianceProcess('git', Arguments, 10000);
  finally
    Arguments.Free;
  end;
  if (not ProcessResult.Started) or (ProcessResult.ExitCode <> 0) then
    raise Exception.CreateFmt('Cannot read suite revision: %s',
      [CombinedProcessMessage(ProcessResult)]);
  ActualRevision := Trim(ProcessResult.StdOutText);
  if not SameText(ActualRevision, AExpectedRevision) then
    raise Exception.CreateFmt('Suite revision mismatch: expected %s, got %s',
      [AExpectedRevision, ActualRevision]);
end;

function ReadOptionValue(const AName: string; out AValue: string): Boolean;
var
  I: Integer;
  Prefix: string;
begin
  Prefix := '--' + AName + '=';
  for I := 1 to ParamCount do
  begin
    if StartsText(Prefix, ParamStr(I)) then
    begin
      AValue := Copy(ParamStr(I), Length(Prefix) + 1, MaxInt);
      Exit(True);
    end;
    if SameText(ParamStr(I), '--' + AName) and (I < ParamCount) then
    begin
      AValue := ParamStr(I + 1);
      Exit(True);
    end;
  end;
  AValue := '';
  Result := False;
end;

function HasCommandLineFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), '--' + AName) then
      Exit(True);
  Result := False;
end;

function PositiveIntegerOption(const AName: string;
  const ADefaultValue: Integer): Integer;
var
  Value: string;
begin
  if not ReadOptionValue(AName, Value) then
    Exit(ADefaultValue);
  if (not TryStrToInt(Value, Result)) or (Result <= 0) then
    raise Exception.CreateFmt('--%s must be a positive integer', [AName]);
end;

function DefaultComplianceJobs: Integer;
begin
  Result := TThread.ProcessorCount;
  if Result < 1 then
    Result := 1;
end;

procedure WriteJSONFile(const APath, AJSON: string);
var
  Directory: string;
begin
  Directory := ExtractFileDir(ExpandFileName(APath));
  if (Directory <> '') and (not DirectoryExists(Directory)) and
     (not ForceDirectories(Directory)) then
    raise Exception.CreateFmt('Cannot create report directory: %s',
      [Directory]);
  WriteUTF8FileText(APath, AJSON + LineEnding);
end;

function CombinedProcessMessage(
  const AResult: TComplianceProcessResult): string;
begin
  Result := Trim(AResult.StdOutText);
  if Trim(AResult.StdErrText) <> '' then
  begin
    if Result <> '' then
      Result := Result + LineEnding;
    Result := Result + Trim(AResult.StdErrText);
  end;
  if AResult.ErrorMessage <> '' then
  begin
    if Result <> '' then
      Result := Result + LineEnding;
    Result := Result + AResult.ErrorMessage;
  end;
end;

constructor TComplianceCoordinator.Create(
  const AExecutor: TComplianceCaseExecutor);
begin
  inherited Create;
  FExecutor := AExecutor;
end;

procedure TComplianceCoordinator.WorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    FExecutor.ExecuteCase(AFileName, AIndex, FResults[AIndex]);
  except
    on E: Exception do
    begin
      FResults[AIndex].ID := AFileName;
      FResults[AIndex].Outcome := coInfrastructure;
      FResults[AIndex].MessageText := E.ClassName + ': ' + E.Message;
    end;
  end;
end;

function TComplianceCoordinator.Run(const ACases: TStringList;
  const AJobCount: Integer): TComplianceCaseResultArray;
var
  I, JobCount: Integer;
  Pool: TGocciaThreadPool;
begin
  SetLength(FResults, ACases.Count);
  for I := 0 to High(FResults) do
  begin
    FResults[I].ID := ACases[I];
    FResults[I].Outcome := coInfrastructure;
    FResults[I].MessageText := 'case was not executed';
  end;
  JobCount := Min(Max(1, AJobCount), Max(1, ACases.Count));
  Pool := TGocciaThreadPool.Create(JobCount);
  try
    Pool.RunAll(ACases, WorkerProc);
  finally
    Pool.Free;
  end;
  SetLength(Result, Length(FResults));
  for I := 0 to High(FResults) do
    Result[I] := FResults[I];
end;

end.
