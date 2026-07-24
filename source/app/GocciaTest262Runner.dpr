program GocciaTest262Runner;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads, BaseUnix,{$ENDIF}
  Classes,
  Generics.Collections,
  Generics.Defaults,
  Math,
  StrUtils,
  SysUtils,

  FileUtils,
  TextSemantics,
  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Executor,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.JSON.Utils,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.StackLimit,
  Goccia.Test262.Host,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Threading.Init,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

const
  DEFAULT_CATEGORIES = 'built-ins,harness,intl402,language,staging';
  DEFAULT_JOBS = 4;
  DEFAULT_TIMEOUT_MS = 20000;
  DEFAULT_MAX_MEMORY_BYTES = Int64(2) * 1024 * 1024 * 1024;
  DEFAULT_STACK_DEPTH = 10000;
  FNV_OFFSET_BASIS = UInt32($811C9DC5);
  FNV_PRIME = UInt32($01000193);
  ASYNC_PASS_MARKER = 'Test262:AsyncTestComplete';
  ASYNC_FAIL_MARKER = 'Test262:AsyncTestFailure:';
  NEGATIVE_NO_ERROR_MARKER = 'Test262:NegativeTestNoError';
  NEGATIVE_ERROR_MARKER = 'Test262:NegativeTestError:';
  WORKER_RESULT_VERSION = 1;

type
  TTest262Outcome = (toPass, toFail, toWrapperInfra, toTimeout);
  TIntegerArray = array of Integer;
  TTest262WrapperKind = (twPositiveSync, twPositiveAsync,
    twNegativeRuntime, twNegativeRuntimeUnwrapped, twNegativeParse);
  TTest262ProfileMode = (tpmOpcodes, tpmFunctions, tpmAll);

  TTest262Metadata = class
  public
    Flags: TStringList;
    Includes: TStringList;
    Features: TStringList;
    NegativePhase: string;
    NegativeType: string;
    constructor Create;
    destructor Destroy; override;
    function HasFlag(const AName: string): Boolean;
    function HasFeature(const AName: string): Boolean;
    function IsNegative: Boolean;
  end;

  TTest262Case = class
  public
    Id: string;
    Path: string;
  end;

  TTest262Result = record
    Id: string;
    Status: TTest262Outcome;
    DurationMs: Integer;
    Message: string;
    Diagnostic: string;
    ProfilePath: string;
    ProfileMissing: Boolean;
  end;

  TTest262Options = record
    SuiteDir: string;
    OutputPath: string;
    ProfileDir: string;
    Filter: string;
    Categories: string;
    Jobs: Integer;
    MaxTests: Integer;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    Mode: TGocciaTest262ExecutionMode;
    ProfileMode: TTest262ProfileMode;
    ShardIndex: Integer;
    ShardCount: Integer;
    Verbose: Boolean;
    HostEval: Boolean;
    HostEvalDeterministic: Boolean;
    HostEvalWarningUnsupportedFeatures: Boolean;
    HostEvalSourceType: TGocciaSourceType;
  end;

  TTest262PrintHost = class
  private
    FLines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function PrintValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Text: string;
  end;

  TTest262App = class
  private
    FOptions: TTest262Options;
    FCases: TObjectList<TTest262Case>;
    FSelectedPaths: TStringList;
    FResults: array of TTest262Result;
    FTotalDiscovered: Integer;
    FDurationNanoseconds: Int64;
    procedure ParseArguments;
    procedure PrintUsage;
    procedure Discover;
    function NormalizeId(const APath: string): string;
    function MatchesFilter(const AId: string): Boolean;
    function ShardForTest(const AId: string): Integer;
    function ParseTest(const ASource: string; out ABody: string;
      out AMetadata: TTest262Metadata): Boolean;
    function BuildHarness(const AMetadata: TTest262Metadata;
      const AIsAsync, ARaw: Boolean): string;
    function BuildSource(const AHarness, ABody: string;
      const AKind: TTest262WrapperKind;
      const AStrictMode: Boolean): string;
    function DetermineKind(const ACase: TTest262Case;
      const AMetadata: TTest262Metadata): TTest262WrapperKind;
    procedure ConfigureProfiler;
    procedure WarmUpRuntime(const AEngine: TGocciaEngine);
    function ProfilePathForTest(const AId: string): string;
    procedure ExecuteOne(const ACase: TTest262Case;
      out AResult: TTest262Result);
    procedure WorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
    procedure ExecuteAll;
    procedure PrintSummary;
    procedure WriteReport;
    function RunHostEval: Integer;
    function FailureCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Run: Integer;
  end;

function OutcomeName(const AOutcome: TTest262Outcome): string;
begin
  case AOutcome of
    toPass: Result := 'PASS';
    toFail: Result := 'FAIL';
    toWrapperInfra: Result := 'WRAPPER_INFRA';
    toTimeout: Result := 'TIMEOUT';
  end;
end;

procedure WriteWorkerString(const AStream: TStream; const AValue: string);
var
  ByteLength: LongInt;
  ValueBytes: UTF8String;
begin
  ValueBytes := UTF8String(AValue);
  ByteLength := Length(ValueBytes);
  AStream.WriteBuffer(ByteLength, SizeOf(ByteLength));
  if ByteLength > 0 then
    AStream.WriteBuffer(ValueBytes[1], ByteLength);
end;

function ReadWorkerString(const AStream: TStream): string;
var
  ByteLength: LongInt;
  ValueBytes: UTF8String;
begin
  AStream.ReadBuffer(ByteLength, SizeOf(ByteLength));
  if (ByteLength < 0) or (ByteLength > AStream.Size - AStream.Position) then
    raise EReadError.Create('Invalid worker result string length');
  SetLength(ValueBytes, ByteLength);
  if ByteLength > 0 then
    AStream.ReadBuffer(ValueBytes[1], ByteLength);
  Result := string(ValueBytes);
end;

procedure WriteWorkerResult(const APath: string;
  const AResult: TTest262Result);
var
  DurationMs: LongInt;
  Outcome: LongInt;
  ProfileMissing: Byte;
  Stream: TFileStream;
  Version: LongInt;
begin
  Stream := TFileStream.Create(APath, fmCreate);
  try
    Version := WORKER_RESULT_VERSION;
    Outcome := Ord(AResult.Status);
    DurationMs := AResult.DurationMs;
    ProfileMissing := Ord(AResult.ProfileMissing);
    Stream.WriteBuffer(Version, SizeOf(Version));
    Stream.WriteBuffer(Outcome, SizeOf(Outcome));
    Stream.WriteBuffer(DurationMs, SizeOf(DurationMs));
    Stream.WriteBuffer(ProfileMissing, SizeOf(ProfileMissing));
    WriteWorkerString(Stream, AResult.Id);
    WriteWorkerString(Stream, AResult.Message);
    WriteWorkerString(Stream, AResult.Diagnostic);
    WriteWorkerString(Stream, AResult.ProfilePath);
  finally
    Stream.Free;
  end;
end;

procedure ReadWorkerResult(const APath: string;
  out AResult: TTest262Result);
var
  DurationMs: LongInt;
  Outcome: LongInt;
  ProfileMissing: Byte;
  Stream: TFileStream;
  Version: LongInt;
begin
  Stream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    Stream.ReadBuffer(Version, SizeOf(Version));
    if Version <> WORKER_RESULT_VERSION then
      raise EReadError.Create('Unsupported worker result version');
    Stream.ReadBuffer(Outcome, SizeOf(Outcome));
    if (Outcome < Ord(Low(TTest262Outcome))) or
       (Outcome > Ord(High(TTest262Outcome))) then
      raise EReadError.Create('Invalid worker result outcome');
    Stream.ReadBuffer(DurationMs, SizeOf(DurationMs));
    Stream.ReadBuffer(ProfileMissing, SizeOf(ProfileMissing));
    AResult := Default(TTest262Result);
    AResult.Status := TTest262Outcome(Outcome);
    AResult.DurationMs := DurationMs;
    AResult.ProfileMissing := ProfileMissing <> 0;
    AResult.Id := ReadWorkerString(Stream);
    AResult.Message := ReadWorkerString(Stream);
    AResult.Diagnostic := ReadWorkerString(Stream);
    AResult.ProfilePath := ReadWorkerString(Stream);
  finally
    Stream.Free;
  end;
end;

function FirstLine(const AText: string): string;
var
  LineEnd: SizeInt;
begin
  LineEnd := Pos(#10, AText);
  if LineEnd > 0 then
    Result := Trim(Copy(AText, 1, LineEnd - 1))
  else
    Result := Trim(AText);
  if Length(Result) > 200 then
    SetLength(Result, 200);
end;

function ExtractMarkerValue(const AText, AMarker: string;
  out AValue: string): Boolean;
var
  MarkerPos: SizeInt;
  LineEnd: SizeInt;
begin
  MarkerPos := Pos(AMarker, AText);
  Result := MarkerPos > 0;
  if not Result then
  begin
    AValue := '';
    Exit;
  end;
  AValue := Copy(AText, MarkerPos + Length(AMarker), MaxInt);
  LineEnd := Pos(#10, AValue);
  if LineEnd > 0 then
    SetLength(AValue, LineEnd - 1);
  AValue := Trim(AValue);
end;

function StripQuotesAndComma(const AValue: string): string;
begin
  Result := Trim(AValue);
  if (Result <> '') and (Result[Length(Result)] = ',') then
    Delete(Result, Length(Result), 1);
  Result := Trim(Result);
  if (Length(Result) >= 2) and
     (((Result[1] = '''') and
       (Result[Length(Result)] = '''')) or
      ((Result[1] = '"') and (Result[Length(Result)] = '"'))) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure AddInlineList(const AList: TStrings; const AValue: string);
var
  DelimiterPosition: Integer;
  Item: string;
  ListText: string;
begin
  ListText := Trim(AValue);
  if ListText = '' then
    Exit;
  if ListText[1] = '[' then
  begin
    Delete(ListText, 1, 1);
    DelimiterPosition := Pos(']', ListText);
    if DelimiterPosition > 0 then
      ListText := Copy(ListText, 1, DelimiterPosition - 1);
  end;
  repeat
    DelimiterPosition := Pos(',', ListText);
    if DelimiterPosition > 0 then
    begin
      Item := Copy(ListText, 1, DelimiterPosition - 1);
      Delete(ListText, 1, DelimiterPosition);
    end
    else
    begin
      Item := ListText;
      ListText := '';
    end;
    Item := StripQuotesAndComma(Item);
    if (Item <> '') and (AList.IndexOf(Item) < 0) then
      AList.Add(Item);
  until DelimiterPosition = 0;
end;

constructor TTest262Metadata.Create;
begin
  inherited Create;
  Flags := TStringList.Create;
  Includes := TStringList.Create;
  Features := TStringList.Create;
end;

destructor TTest262Metadata.Destroy;
begin
  Features.Free;
  Includes.Free;
  Flags.Free;
  inherited;
end;

function TTest262Metadata.HasFlag(const AName: string): Boolean;
begin
  Result := Flags.IndexOf(AName) >= 0;
end;

function TTest262Metadata.HasFeature(const AName: string): Boolean;
begin
  Result := Features.IndexOf(AName) >= 0;
end;

function TTest262Metadata.IsNegative: Boolean;
begin
  Result := (NegativePhase <> '') or (NegativeType <> '');
end;

constructor TTest262PrintHost.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor TTest262PrintHost.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TTest262PrintHost.PrintValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Line: string;
begin
  Line := '';
  for I := 0 to AArgs.Length - 1 do
  begin
    if I > 0 then
      Line := Line + ' ';
    Line := Line + AArgs.GetElement(I).ToStringLiteral.Value;
  end;
  FLines.Add(Line);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TTest262PrintHost.Text: string;
begin
  Result := FLines.Text;
end;

constructor TTest262App.Create;
begin
  inherited Create;
  FOptions := Default(TTest262Options);
  FOptions.Categories := DEFAULT_CATEGORIES;
  FOptions.Jobs := DEFAULT_JOBS;
  FOptions.TimeoutMs := DEFAULT_TIMEOUT_MS;
  FOptions.MaxMemoryBytes := DEFAULT_MAX_MEMORY_BYTES;
  FOptions.Mode := t262emBytecode;
  FOptions.ProfileMode := tpmAll;
  FOptions.ShardIndex := -1;
  FOptions.ShardCount := 0;
  FOptions.HostEvalSourceType := stScript;
  FCases := TObjectList<TTest262Case>.Create(True);
  FSelectedPaths := TStringList.Create;
end;

destructor TTest262App.Destroy;
begin
  FSelectedPaths.Free;
  FCases.Free;
  inherited;
end;

procedure TTest262App.PrintUsage;
begin
  WriteLn('Usage: GocciaTest262Runner --suite-dir=DIR [options]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --suite-dir=DIR              Existing test262 checkout');
  WriteLn('  --output=FILE                JSON result report');
  WriteLn('  --profile-dir=DIR            Per-test VM profile directory');
  WriteLn('  --profile-mode=MODE          opcodes, functions, or all');
  WriteLn('  --categories=LIST            Comma-separated top-level categories');
  WriteLn('  --filter=GLOB                Test ID glob');
  WriteLn('  --max-tests=N                Cap before sharding (0 = unlimited)');
  WriteLn('  --jobs=N                     Concurrent native workers (default: 4)');
  WriteLn('  --mode=interpreted|bytecode  Execution mode (default: bytecode)');
  WriteLn('  --timeout-ms=N               Per-test cooperative timeout');
  WriteLn('  --max-memory=N               Per-test GC heap ceiling');
  WriteLn('  --shard-index=N              Zero-based shard index');
  WriteLn('  --shard-count=N              Total shard count');
  WriteLn('  --verbose                    Print every test result');
  WriteLn('  --eval-host                  Execute a Test262 host probe from stdin');
  WriteLn('  --deterministic              Stable host state for --eval-host');
  WriteLn('  --warning-unsupported-features');
  WriteLn('                               Parser warnings for --eval-host');
end;

procedure TTest262App.ParseArguments;
var
  Argument: string;
  I: Integer;
  Value: string;

  function ArgumentValue(const AName: string): string;
  begin
    if StartsStr(AName + '=', Argument) then
      Result := Copy(Argument, Length(AName) + 2, MaxInt)
    else
    begin
      Inc(I);
      if I > ParamCount then
        raise Exception.Create(AName + ' requires a value');
      Result := ParamStr(I);
    end;
    if Result = '' then
      raise Exception.Create(AName + ' requires a value');
  end;

  function NonNegativeInteger(const AName, ARaw: string): Integer;
  begin
    if not TryStrToInt(ARaw, Result) or (Result < 0) then
      raise Exception.Create(AName +
        ' requires a non-negative integer, got: ' + ARaw);
  end;

  function PositiveInteger(const AName, ARaw: string): Integer;
  begin
    Result := NonNegativeInteger(AName, ARaw);
    if Result = 0 then
      raise Exception.Create(AName +
        ' requires a positive integer, got: ' + ARaw);
  end;

begin
  I := 1;
  while I <= ParamCount do
  begin
    Argument := ParamStr(I);
    if (Argument = '--help') or (Argument = '-h') then
    begin
      PrintUsage;
      Halt(0);
    end
    else if (Argument = '--verbose') or (Argument = '-v') then
      FOptions.Verbose := True
    else if Argument = '--eval-host' then
      FOptions.HostEval := True
    else if Argument = '--deterministic' then
      FOptions.HostEvalDeterministic := True
    else if Argument = '--warning-unsupported-features' then
      FOptions.HostEvalWarningUnsupportedFeatures := True
    else if StartsStr('--compat-', Argument) or
        (Argument = '--unsafe-function-constructor') or
        (Argument = '--unsafe-shadowrealm') then
    begin
      { Host-eval compatibility flags are already part of the Test262 profile. }
    end
    else if (Argument = '--source-type') or
        StartsStr('--source-type=', Argument) then
    begin
      Value := ArgumentValue('--source-type');
      if Value = 'script' then
        FOptions.HostEvalSourceType := stScript
      else if Value = 'module' then
        FOptions.HostEvalSourceType := stModule
      else
        raise Exception.Create('--source-type requires script or module');
    end
    else if (Argument = '--suite-dir') or
        StartsStr('--suite-dir=', Argument) then
      FOptions.SuiteDir := ExpandFileName(ArgumentValue('--suite-dir'))
    else if (Argument = '--output') or
        StartsStr('--output=', Argument) then
      FOptions.OutputPath := ExpandFileName(ArgumentValue('--output'))
    else if (Argument = '--profile-dir') or
        StartsStr('--profile-dir=', Argument) then
      FOptions.ProfileDir := ExpandFileName(ArgumentValue('--profile-dir'))
    else if (Argument = '--filter') or StartsStr('--filter=', Argument) then
      FOptions.Filter := ArgumentValue('--filter')
    else if (Argument = '--categories') or
        StartsStr('--categories=', Argument) then
      FOptions.Categories := ArgumentValue('--categories')
    else if (Argument = '--jobs') or StartsStr('--jobs=', Argument) then
      FOptions.Jobs := PositiveInteger('--jobs', ArgumentValue('--jobs'))
    else if (Argument = '--max-tests') or
        StartsStr('--max-tests=', Argument) then
      FOptions.MaxTests := NonNegativeInteger('--max-tests',
        ArgumentValue('--max-tests'))
    else if (Argument = '--timeout-ms') or
        StartsStr('--timeout-ms=', Argument) then
      FOptions.TimeoutMs := PositiveInteger('--timeout-ms',
        ArgumentValue('--timeout-ms'))
    else if (Argument = '--max-memory') or
        StartsStr('--max-memory=', Argument) then
    begin
      Value := ArgumentValue('--max-memory');
      if not TryStrToInt64(Value, FOptions.MaxMemoryBytes) or
         (FOptions.MaxMemoryBytes <= 0) then
        raise Exception.Create('--max-memory requires a positive integer');
    end
    else if (Argument = '--shard-index') or
        StartsStr('--shard-index=', Argument) then
      FOptions.ShardIndex := NonNegativeInteger('--shard-index',
        ArgumentValue('--shard-index'))
    else if (Argument = '--shard-count') or
        StartsStr('--shard-count=', Argument) then
      FOptions.ShardCount := PositiveInteger('--shard-count',
        ArgumentValue('--shard-count'))
    else if (Argument = '--mode') or StartsStr('--mode=', Argument) then
    begin
      Value := ArgumentValue('--mode');
      if Value = 'interpreted' then
        FOptions.Mode := t262emInterpreted
      else if Value = 'bytecode' then
        FOptions.Mode := t262emBytecode
      else
        raise Exception.Create(
          '--mode requires interpreted or bytecode');
    end
    else if (Argument = '--profile-mode') or
        StartsStr('--profile-mode=', Argument) then
    begin
      Value := ArgumentValue('--profile-mode');
      if Value = 'opcodes' then
        FOptions.ProfileMode := tpmOpcodes
      else if Value = 'functions' then
        FOptions.ProfileMode := tpmFunctions
      else if Value = 'all' then
        FOptions.ProfileMode := tpmAll
      else
        raise Exception.Create(
          '--profile-mode requires opcodes, functions, or all');
    end
    else
      raise Exception.Create('Unknown argument: ' + Argument);
    Inc(I);
  end;

  if (FOptions.SuiteDir = '') and not FOptions.HostEval then
    raise Exception.Create('--suite-dir is required');
  if not FOptions.HostEval and
     not DirectoryExists(IncludeTrailingPathDelimiter(FOptions.SuiteDir) +
      'test') then
    raise Exception.Create('test262 test directory not found under: ' +
      FOptions.SuiteDir);
  if (FOptions.ShardIndex >= 0) <> (FOptions.ShardCount > 0) then
    raise Exception.Create(
      '--shard-index and --shard-count must be provided together');
  if (FOptions.ShardIndex >= 0) and
     (FOptions.ShardIndex >= FOptions.ShardCount) then
    raise Exception.Create('--shard-index must be less than --shard-count');
  if (FOptions.ProfileDir <> '') and
     (FOptions.Mode <> t262emBytecode) then
    raise Exception.Create('--profile-dir requires --mode=bytecode');
end;

function TTest262App.NormalizeId(const APath: string): string;
var
  Prefix: string;
begin
  Prefix := IncludeTrailingPathDelimiter(FOptions.SuiteDir) +
    'test' + PathDelim;
  Result := Copy(APath, Length(Prefix) + 1, MaxInt);
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

function TTest262App.MatchesFilter(const AId: string): Boolean;
  function MatchFrom(const AText, APattern: string;
    const ATextIndex, APatternIndex: Integer): Boolean;
  var
    DoubleStar: Boolean;
    NextTextIndex: Integer;
  begin
    if APatternIndex > Length(APattern) then
      Exit(ATextIndex > Length(AText));
    if APattern[APatternIndex] = '*' then
    begin
      DoubleStar := (APatternIndex < Length(APattern)) and
        (APattern[APatternIndex + 1] = '*');
      if DoubleStar then
      begin
        for NextTextIndex := ATextIndex to Length(AText) + 1 do
          if MatchFrom(AText, APattern, NextTextIndex,
              APatternIndex + 2) then
            Exit(True);
      end
      else
      begin
        NextTextIndex := ATextIndex;
        repeat
          if MatchFrom(AText, APattern, NextTextIndex,
              APatternIndex + 1) then
            Exit(True);
          if (NextTextIndex > Length(AText)) or
             (AText[NextTextIndex] = '/') then
            Break;
          Inc(NextTextIndex);
        until False;
      end;
      Exit(False);
    end;
    if ATextIndex > Length(AText) then
      Exit(False);
    if (APattern[APatternIndex] = '?') or
       (APattern[APatternIndex] = AText[ATextIndex]) then
      Exit(MatchFrom(AText, APattern, ATextIndex + 1,
        APatternIndex + 1));
    Result := False;
  end;

var
  SeparatorIndex: Integer;
begin
  if FOptions.Filter = '' then
    Exit(True);
  if MatchFrom(AId, FOptions.Filter, 1, 1) then
    Exit(True);
  for SeparatorIndex := 1 to Length(AId) do
    if (AId[SeparatorIndex] = '/') and
       MatchFrom(AId, FOptions.Filter, SeparatorIndex + 1, 1) then
      Exit(True);
  Result := False;
end;

function TTest262App.ShardForTest(const AId: string): Integer;
var
  Hash: UInt32;
  I: Integer;
  Product: UInt64;
begin
  Hash := FNV_OFFSET_BASIS;
  for I := 1 to Length(AId) do
  begin
    Hash := Hash xor Ord(AId[I]);
    Product := UInt64(Hash) * UInt64(FNV_PRIME);
    Hash := UInt32(Product and UInt64($FFFFFFFF));
  end;
  Result := Integer(Hash mod UInt32(FOptions.ShardCount));
end;

function CompareTest262Cases(
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft,
  ARight: TTest262Case): Integer;
begin
  Result := CompareText(ALeft.Id, ARight.Id);
end;

procedure TTest262App.Discover;
var
  Candidate: TTest262Case;
  Categories: TStringList;
  CategoryDirectory: string;
  CategoryFiles: TStringList;
  I, J: Integer;
begin
  Categories := TStringList.Create;
  CategoryFiles := TStringList.Create;
  try
    Categories.StrictDelimiter := True;
    Categories.Delimiter := ',';
    Categories.DelimitedText := FOptions.Categories;
    for I := 0 to Categories.Count - 1 do
    begin
      CategoryDirectory := IncludeTrailingPathDelimiter(FOptions.SuiteDir) +
        'test' + PathDelim + Trim(Categories[I]);
      if not DirectoryExists(CategoryDirectory) then
      begin
        WriteLn(ErrOutput, 'Warning: category directory not found: ',
          CategoryDirectory);
        Continue;
      end;
      CategoryFiles.Clear;
      CategoryFiles.AddStrings(FindAllFiles(CategoryDirectory, '.js'));
      for J := 0 to CategoryFiles.Count - 1 do
      begin
        if EndsStr('_FIXTURE.js', CategoryFiles[J]) then
          Continue;
        Candidate := TTest262Case.Create;
        Candidate.Path := ExpandFileName(CategoryFiles[J]);
        Candidate.Id := NormalizeId(Candidate.Path);
        if MatchesFilter(Candidate.Id) then
          FCases.Add(Candidate)
        else
          Candidate.Free;
      end;
    end;
    FCases.Sort(TComparer<TTest262Case>.Construct(CompareTest262Cases));
    if (FOptions.MaxTests > 0) and
       (FCases.Count > FOptions.MaxTests) then
      while FCases.Count > FOptions.MaxTests do
        FCases.Delete(FCases.Count - 1);
    FTotalDiscovered := FCases.Count;
    for I := FCases.Count - 1 downto 0 do
      if (FOptions.ShardCount > 0) and
         (ShardForTest(FCases[I].Id) <> FOptions.ShardIndex) then
        FCases.Delete(I);
    for I := 0 to FCases.Count - 1 do
      FSelectedPaths.Add(FCases[I].Path);
  finally
    CategoryFiles.Free;
    Categories.Free;
  end;
end;

function TTest262App.ParseTest(const ASource: string; out ABody: string;
  out AMetadata: TTest262Metadata): Boolean;
var
  Block: string;
  BlockStart: SizeInt;
  ClosePosition: SizeInt;
  ColonPosition: SizeInt;
  CurrentKey: string;
  HeaderLines: TStringList;
  I: Integer;
  Line: string;
  OpenPosition: SizeInt;
  TopLevel: Boolean;
  Trimmed: string;
  Value: string;
begin
  AMetadata := TTest262Metadata.Create;
  OpenPosition := Pos('/*---', ASource);
  ClosePosition := 0;
  if OpenPosition > 0 then
    ClosePosition := PosEx('---*/', ASource,
      OpenPosition + Length('/*---'));
  if (OpenPosition = 0) or (ClosePosition = 0) then
  begin
    ABody := ASource;
    Exit(True);
  end;

  BlockStart := OpenPosition + Length('/*---');
  if Copy(ASource, BlockStart, 2) = #13#10 then
    Inc(BlockStart, 2)
  else if CharInSet(ASource[BlockStart], [#10, #13]) then
    Inc(BlockStart);
  Block := Copy(ASource, BlockStart, ClosePosition - BlockStart);
  ABody := TrimRight(Copy(ASource, 1, OpenPosition - 1)) + sLineBreak +
    Copy(ASource, ClosePosition + Length('---*/'), MaxInt);
  while (ABody <> '') and CharInSet(ABody[1], [#10, #13]) do
    Delete(ABody, 1, 1);

  HeaderLines := TStringList.Create;
  try
    HeaderLines.Text := Block;
    CurrentKey := '';
    for I := 0 to HeaderLines.Count - 1 do
    begin
      Line := HeaderLines[I];
      Trimmed := Trim(Line);
      if (Trimmed = '') or StartsStr('#', Trimmed) then
        Continue;
      TopLevel := not CharInSet(Line[1], [' ', #9]);
      if TopLevel then
      begin
        ColonPosition := Pos(':', Line);
        if ColonPosition = 0 then
        begin
          CurrentKey := '';
          Continue;
        end;
        CurrentKey := Trim(Copy(Line, 1, ColonPosition - 1));
        Value := Trim(Copy(Line, ColonPosition + 1, MaxInt));
        if CurrentKey = 'flags' then
          AddInlineList(AMetadata.Flags, Value)
        else if CurrentKey = 'includes' then
          AddInlineList(AMetadata.Includes, Value)
        else if CurrentKey = 'features' then
          AddInlineList(AMetadata.Features, Value)
        else if CurrentKey <> 'negative' then
          CurrentKey := '';
        Continue;
      end;

      if StartsStr('-', Trimmed) then
      begin
        Value := StripQuotesAndComma(Trim(Copy(Trimmed, 2, MaxInt)));
        if CurrentKey = 'flags' then
          AMetadata.Flags.Add(Value)
        else if CurrentKey = 'includes' then
          AMetadata.Includes.Add(Value)
        else if CurrentKey = 'features' then
          AMetadata.Features.Add(Value);
        Continue;
      end;

      if CurrentKey = 'negative' then
      begin
        ColonPosition := Pos(':', Trimmed);
        if ColonPosition > 0 then
        begin
          Value := StripQuotesAndComma(
            Copy(Trimmed, ColonPosition + 1, MaxInt));
          if Trim(Copy(Trimmed, 1, ColonPosition - 1)) = 'phase' then
            AMetadata.NegativePhase := Value
          else if Trim(Copy(Trimmed, 1, ColonPosition - 1)) = 'type' then
            AMetadata.NegativeType := Value;
        end;
      end;
    end;
  finally
    HeaderLines.Free;
  end;
  Result := True;
end;

function TTest262App.BuildHarness(const AMetadata: TTest262Metadata;
  const AIsAsync, ARaw: Boolean): string;
var
  HarnessNames: TStringList;
  I: Integer;
  Name: string;
  Path: string;
begin
  Result := '';
  if ARaw then
    Exit;
  HarnessNames := TStringList.Create;
  try
    HarnessNames.Add('$262.js');
    HarnessNames.Add('sta.js');
    HarnessNames.Add('assert.js');
    for I := 0 to AMetadata.Includes.Count - 1 do
      if HarnessNames.IndexOf(AMetadata.Includes[I]) < 0 then
        HarnessNames.Add(AMetadata.Includes[I]);
    if AIsAsync and (HarnessNames.IndexOf('doneprintHandle.js') < 0) then
      HarnessNames.Add('doneprintHandle.js');
    HarnessNames.Add('goccia-global-shim.js');

    for I := 0 to HarnessNames.Count - 1 do
    begin
      Name := HarnessNames[I];
      if (Name = '$262.js') or (Name = 'goccia-global-shim.js') then
        Path := ExpandFileName('scripts' + PathDelim +
          'test262_harness' + PathDelim + Name)
      else
        Path := IncludeTrailingPathDelimiter(FOptions.SuiteDir) +
          'harness' + PathDelim + Name;
      if not FileExists(Path) then
        raise Exception.Create('Missing harness include "' + Name +
          '": ' + Path);
      if Result <> '' then
        Result := Result + sLineBreak;
      Result := Result + ReadUTF8FileText(Path);
    end;
  finally
    HarnessNames.Free;
  end;
end;

function TTest262App.BuildSource(const AHarness, ABody: string;
  const AKind: TTest262WrapperKind;
  const AStrictMode: Boolean): string;
var
  Prefix: string;
begin
  if AStrictMode then
    Prefix := '"use strict";' + sLineBreak
  else
    Prefix := '';

  if AKind = twNegativeParse then
    Exit(Prefix + ABody);
  if AKind = twNegativeRuntimeUnwrapped then
    Exit(Prefix + AHarness + sLineBreak + ABody);
  if AKind = twNegativeRuntime then
    Exit(Prefix + AHarness + sLineBreak +
      'try {' + sLineBreak +
      ABody + sLineBreak +
      '  print("Test262:NegativeTestNoError");' + sLineBreak +
      '} catch (__gocciaT262_e) {' + sLineBreak +
      '  var __gocciaT262_n = "unknown";' + sLineBreak +
      '  if (__gocciaT262_e && typeof __gocciaT262_e === "object") {' +
      sLineBreak +
      '    if (__gocciaT262_e.constructor && ' +
      '__gocciaT262_e.constructor.name) {' + sLineBreak +
      '      __gocciaT262_n = __gocciaT262_e.constructor.name;' +
      sLineBreak +
      '    }' + sLineBreak +
      '  }' + sLineBreak +
      '  print("Test262:NegativeTestError:" + __gocciaT262_n);' +
      sLineBreak +
      '}' + sLineBreak);
  Result := Prefix + AHarness;
  if (Result <> '') and not EndsStr(sLineBreak, Result) then
    Result := Result + sLineBreak;
  Result := Result + ABody;
end;

function TTest262App.DetermineKind(const ACase: TTest262Case;
  const AMetadata: TTest262Metadata): TTest262WrapperKind;
begin
  if AMetadata.IsNegative then
  begin
    if (AMetadata.NegativePhase = 'parse') or
       (AMetadata.NegativePhase = 'early') or
       (AMetadata.NegativePhase = 'resolution') or
       AMetadata.HasFlag('module') then
      Exit(twNegativeParse);
    if (AMetadata.NegativePhase = 'runtime') and
       (AMetadata.NegativeType = 'SyntaxError') and
       StartsStr('language/global-code/', ACase.Id) then
      Exit(twNegativeRuntimeUnwrapped);
    Exit(twNegativeRuntime);
  end;
  if AMetadata.HasFlag('async') then
    Result := twPositiveAsync
  else
    Result := twPositiveSync;
end;

procedure TTest262App.ConfigureProfiler;
begin
  if FOptions.ProfileDir = '' then
    Exit;
  TGocciaProfiler.Initialize;
  TGocciaProfiler.Instance.Enabled := True;
  case FOptions.ProfileMode of
    tpmOpcodes:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes];
    tpmFunctions:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmFunctions];
    tpmAll:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes,
        Goccia.Profiler.pmFunctions];
  end;
end;

function TTest262App.ProfilePathForTest(const AId: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FOptions.ProfileDir) +
    'per-test' + PathDelim +
    StringReplace(AId, '/', PathDelim, [rfReplaceAll]) +
    '.profile.json';
end;

procedure ClassifyResult(const AKind: TTest262WrapperKind;
  const AExpectedErrorType, AOutput, AErrorName, ADiagnostic: string;
  const AExecutionFailed, ATimedOut: Boolean;
  out AOutcome: TTest262Outcome; out AMessage, AResultDiagnostic: string);
var
  Actual: string;
begin
  AMessage := '';
  AResultDiagnostic := '';
  if ATimedOut then
  begin
    AOutcome := toTimeout;
    AMessage := 'engine timeout';
    Exit;
  end;
  if AKind = twNegativeParse then
  begin
    if AExecutionFailed then
      AOutcome := toPass
    else
    begin
      AOutcome := toFail;
      AMessage := 'expected parse error, body executed cleanly';
    end;
    Exit;
  end;

  if AKind = twNegativeRuntime then
  begin
    if Pos(NEGATIVE_NO_ERROR_MARKER, AOutput) > 0 then
    begin
      AOutcome := toFail;
      if AExpectedErrorType <> '' then
        AMessage := 'expected ' + AExpectedErrorType + ', none thrown'
      else
        AMessage := 'expected error, none thrown';
      Exit;
    end;
    if ExtractMarkerValue(AOutput, NEGATIVE_ERROR_MARKER, Actual) then
    begin
      if (AExpectedErrorType <> '') and
         (Actual <> AExpectedErrorType) then
      begin
        AOutcome := toFail;
        AMessage := 'expected ' + AExpectedErrorType + ', got ' + Actual;
      end
      else
        AOutcome := toPass;
      Exit;
    end;
    AOutcome := toWrapperInfra;
    AMessage := 'negative-runtime wrapper produced no marker';
    AResultDiagnostic := Copy(ADiagnostic, 1, 800);
    Exit;
  end;

  if AKind = twNegativeRuntimeUnwrapped then
  begin
    if not AExecutionFailed then
    begin
      AOutcome := toFail;
      if AExpectedErrorType <> '' then
        AMessage := 'expected ' + AExpectedErrorType + ', none thrown'
      else
        AMessage := 'expected error, none thrown';
    end
    else if (AExpectedErrorType <> '') and
            (AErrorName <> AExpectedErrorType) then
    begin
      AOutcome := toFail;
      AMessage := 'expected ' + AExpectedErrorType + ', got ' + AErrorName;
      AResultDiagnostic := Copy(ADiagnostic, 1, 300);
    end
    else
      AOutcome := toPass;
    Exit;
  end;

  if AKind = twPositiveAsync then
  begin
    if Pos(ASYNC_PASS_MARKER, AOutput) > 0 then
      AOutcome := toPass
    else if ExtractMarkerValue(AOutput, ASYNC_FAIL_MARKER, Actual) then
    begin
      AOutcome := toFail;
      AMessage := Copy(Actual, 1, 200);
    end
    else
    begin
      AOutcome := toFail;
      if AExecutionFailed then
        AMessage := 'async test threw before $DONE'
      else
        AMessage := 'async test did not call $DONE';
      AResultDiagnostic := Copy(ADiagnostic, 1, 300);
    end;
    Exit;
  end;

  if AExecutionFailed then
  begin
    AOutcome := toFail;
    AMessage := FirstLine(ADiagnostic);
    AResultDiagnostic := Copy(ADiagnostic, 1, 800);
  end
  else
    AOutcome := toPass;
end;

procedure TTest262App.ExecuteOne(const ACase: TTest262Case;
  out AResult: TTest262Result);
var
  Body: string;
  Diagnostic: string;
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  ErrorColumn: Integer;
  ErrorFileName: string;
  ErrorLine: Integer;
  ErrorMessage: string;
  ErrorName: string;
  ExecutionFailed: Boolean;
  Executor: TGocciaExecutor;
  Harness: string;
  Host: TGocciaTest262Host;
  Kind: TTest262WrapperKind;
  Metadata: TTest262Metadata;
  PrintHost: TTest262PrintHost;
  ProfilePath: string;
  Source: TStringList;
  SourceText: string;
  StartNanoseconds: Int64;
  TimedOut: Boolean;
begin
  AResult := Default(TTest262Result);
  AResult.Id := ACase.Id;
  StartNanoseconds := GetNanoseconds;
  Metadata := nil;
  Source := nil;
  Executor := nil;
  Engine := nil;
  Host := nil;
  PrintHost := nil;
  ExecutionFailed := False;
  TimedOut := False;
  ErrorName := '';
  Diagnostic := '';
  ProfilePath := '';
  try
    try
      if not ParseTest(ReadUTF8FileText(ACase.Path), Body, Metadata) then
        raise Exception.Create('frontmatter parse failed');
      Kind := DetermineKind(ACase, Metadata);
      Harness := BuildHarness(Metadata, Metadata.HasFlag('async'),
        Metadata.HasFlag('raw') or (Kind = twNegativeParse));
      SourceText := BuildSource(Harness, Body, Kind,
        Metadata.HasFlag('onlyStrict'));
      Source := CreateECMAScriptSourceLines(SourceText);

      EngineOptions := Default(TGocciaTest262EngineOptions);
      EngineOptions.Compatibility := [cfASI, cfVar, cfFunction,
        cfTraditionalFor, cfWhileLoops, cfLooseEquality, cfArgumentsObject,
        cfLabel, cfForIn];
      if not Metadata.HasFlag('module') then
        Include(EngineOptions.Compatibility, cfNonStrictMode);
      EngineOptions.ExperimentalJSModuleSourceEnabled :=
        Metadata.HasFeature('source-phase-imports-module-source');
      if EngineOptions.ExperimentalJSModuleSourceEnabled then
        Include(EngineOptions.Compatibility,
          cfExperimentalJSModuleSource);
      EngineOptions.UnsafeShadowRealm :=
        Metadata.HasFeature('ShadowRealm') or
        Metadata.HasFeature('realms-tests');
      EngineOptions.AgentCanSuspend :=
        Test262SourceCanSuspendAgent(ACase.Path);
      EngineOptions.Mode := FOptions.Mode;
      if Metadata.HasFlag('module') then
        EngineOptions.SourceType := stModule
      else
        EngineOptions.SourceType := stScript;
      EngineOptions.TimeoutMs := FOptions.TimeoutMs;
      EngineOptions.MaxMemoryBytes := FOptions.MaxMemoryBytes;
      EngineOptions.StackSize := DEFAULT_STACK_DEPTH;

      StartExecutionTimeout(FOptions.TimeoutMs);
      StartInstructionLimit(0);
      SetMaxStackDepth(DEFAULT_STACK_DEPTH);
      ConfigureProfiler;
      if FOptions.ProfileDir <> '' then
      begin
        ProfilePath := ProfilePathForTest(ACase.Id);
        ForceDirectories(ExtractFileDir(ProfilePath));
      end;

      Executor := CreateTest262Executor(FOptions.Mode);
      Engine := TGocciaEngine.Create(ACase.Path, Source, Executor);
      ConfigureTest262Engine(Engine, Executor, EngineOptions);
      Host := TGocciaTest262Host.Create(EngineOptions);
      Host.ConfigureHostEnvironment(Engine.HostEnvironment);
      PrintHost := TTest262PrintHost.Create;
      Engine.RegisterGlobal('print',
        TGocciaNativeFunctionValue.CreateWithoutPrototype(
          PrintHost.PrintValue, 'print', -1));
      Host.Install(Engine);
      try
        Engine.Execute;
      except
        on E: TGocciaTimeoutError do
        begin
          TimedOut := True;
          ExecutionFailed := True;
          ErrorName := 'Timeout';
          Diagnostic := E.Message;
        end;
        on E: EGocciaBytecodeThrow do
        begin
          ExecutionFailed := True;
          ExtractThrowLocation(E.ThrownValue, ErrorName, ErrorMessage,
            ErrorFileName, ErrorLine, ErrorColumn);
          Diagnostic := FormatThrowDetail(E.ThrownValue, ACase.Path,
            Source, False);
        end;
        on E: TGocciaThrowValue do
        begin
          ExecutionFailed := True;
          ExtractThrowLocation(E.Value, ErrorName, ErrorMessage,
            ErrorFileName, ErrorLine, ErrorColumn);
          Diagnostic := FormatThrowDetail(E.Value, ACase.Path, Source,
            False, E.Suggestion);
        end;
        on E: TGocciaError do
        begin
          ExecutionFailed := True;
          ErrorName := ErrorDisplayName(E);
          Diagnostic := E.GetDetailedMessage(False);
        end;
        on E: Exception do
        begin
          ExecutionFailed := True;
          ErrorName := E.ClassName;
          Diagnostic := E.ClassName + ': ' + E.Message;
        end;
      end;

      ClassifyResult(Kind, Metadata.NegativeType, PrintHost.Text,
        ErrorName, Diagnostic, ExecutionFailed, TimedOut, AResult.Status,
        AResult.Message, AResult.Diagnostic);
    except
      on E: Exception do
      begin
        AResult.Status := toWrapperInfra;
        AResult.Message := E.Message;
        AResult.Diagnostic := E.ClassName + ': ' + E.Message;
      end;
    end;
  finally
    if (ProfilePath <> '') and Assigned(TGocciaProfiler.Instance) then
    begin
      try
        WriteProfileJSON(TGocciaProfiler.Instance, ProfilePath);
      except
        on E: Exception do
        begin
          AResult.Status := toWrapperInfra;
          AResult.Message := 'profile write failed';
          AResult.Diagnostic := E.ClassName + ': ' + E.Message;
        end;
      end;
    end;
    AResult.ProfilePath := StringReplace(ProfilePath, '\', '/',
      [rfReplaceAll]);
    AResult.ProfileMissing := (ProfilePath <> '') and
      not FileExists(ProfilePath);
    if Assigned(TGocciaProfiler.Instance) then
      TGocciaProfiler.Shutdown;
    PrintHost.Free;
    Host.Free;
    Engine.Free;
    Executor.Free;
    Source.Free;
    Metadata.Free;
    AResult.DurationMs := Round(
      (GetNanoseconds - StartNanoseconds) / 1000000);
  end;
end;

procedure TTest262App.WorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  ExecuteOne(FCases[AIndex], FResults[AIndex]);
end;

procedure TTest262App.WarmUpRuntime(const AEngine: TGocciaEngine);
begin
  WarmUpSharedLazyGlobals(AEngine);
end;

procedure TTest262App.ExecuteAll;
var
  {$IFDEF UNIX}
  ActiveWorkers: Integer;
  ChildPid: TPid;
  CompletedCount: Integer;
  ExitCode: Integer;
  I: Integer;
  NextIndex: Integer;
  ResultIndex: Integer;
  ResultPaths: array of string;
  Slot: Integer;
  SlotIndices: array of Integer;
  SlotPids: array of TPid;
  SlotStartedNanoseconds: array of Int64;
  SlotTimedOut: array of Boolean;
  WaitStatus: cint;
  WorkerMessage: string;
  {$ELSE}
  I: Integer;
  Pool: TGocciaThreadPool;
  {$ENDIF}
  StartNanoseconds: Int64;
begin
  SetLength(FResults, FCases.Count);
  if FCases.Count = 0 then
    Exit;
  EnsureSharedPrototypesInitialized(WarmUpRuntime);
  StartNanoseconds := GetNanoseconds;
  {$IFDEF UNIX}
  SetLength(SlotPids, FOptions.Jobs);
  SetLength(SlotIndices, FOptions.Jobs);
  SetLength(SlotStartedNanoseconds, FOptions.Jobs);
  SetLength(SlotTimedOut, FOptions.Jobs);
  SetLength(ResultPaths, FOptions.Jobs);
  for Slot := 0 to FOptions.Jobs - 1 do
    ResultPaths[Slot] := IncludeTrailingPathDelimiter(GetTempDir(False)) +
      Format('goccia-test262-%d-%d.result', [fpGetPid, Slot]);
  ActiveWorkers := 0;
  CompletedCount := 0;
  NextIndex := 0;
  while CompletedCount < FCases.Count do
  begin
    while (NextIndex < FCases.Count) and
      (ActiveWorkers < FOptions.Jobs) do
    begin
      Slot := 0;
      while SlotPids[Slot] <> 0 do
        Inc(Slot);
      DeleteFile(ResultPaths[Slot]);
      ResultIndex := NextIndex;
      ChildPid := fpFork;
      if ChildPid = 0 then
      begin
        try
          ExecuteOne(FCases[ResultIndex], FResults[ResultIndex]);
          WriteWorkerResult(ResultPaths[Slot], FResults[ResultIndex]);
          fpExit(0);
        except
          fpExit(2);
        end;
      end;
      if ChildPid < 0 then
      begin
        FResults[ResultIndex].Id := FCases[ResultIndex].Id;
        FResults[ResultIndex].Status := toWrapperInfra;
        FResults[ResultIndex].Message := 'Unable to fork Test262 worker';
        FResults[ResultIndex].Diagnostic := FResults[ResultIndex].Message;
        Inc(CompletedCount);
      end
      else
      begin
        SlotPids[Slot] := ChildPid;
        SlotIndices[Slot] := ResultIndex;
        SlotStartedNanoseconds[Slot] := GetNanoseconds;
        SlotTimedOut[Slot] := False;
        Inc(ActiveWorkers);
      end;
      Inc(NextIndex);
    end;

    ChildPid := fpWaitPid(-1, @WaitStatus, WNOHANG);
    if ChildPid = 0 then
    begin
      for Slot := 0 to High(SlotPids) do
        if (SlotPids[Slot] <> 0) and
           (not SlotTimedOut[Slot]) and
           (GetNanoseconds - SlotStartedNanoseconds[Slot] >
             Int64(FOptions.TimeoutMs * 2 + 1000) * 1000000) then
        begin
          SlotTimedOut[Slot] := True;
          fpKill(SlotPids[Slot], SIGKILL);
        end;
      Sleep(1);
      Continue;
    end;
    if ChildPid < 0 then
      Continue;
    Slot := -1;
    for I := 0 to High(SlotPids) do
      if SlotPids[I] = ChildPid then
      begin
        Slot := I;
        Break;
      end;
    if Slot < 0 then
      Continue;
    ResultIndex := SlotIndices[Slot];
    WorkerMessage := '';
    if SlotTimedOut[Slot] then
    begin
      FResults[ResultIndex].Id := FCases[ResultIndex].Id;
      FResults[ResultIndex].Status := toTimeout;
      WorkerMessage := Format(
        'Test262 worker exceeded %dms watchdog',
        [FOptions.TimeoutMs * 2 + 1000]);
    end
    else if WIFEXITED(WaitStatus) then
    begin
      ExitCode := WEXITSTATUS(WaitStatus);
      if (ExitCode = 0) and FileExists(ResultPaths[Slot]) then
        try
          ReadWorkerResult(ResultPaths[Slot], FResults[ResultIndex]);
        except
          on E: Exception do
            WorkerMessage := 'Invalid worker result: ' + E.Message;
        end
      else
        WorkerMessage := Format(
          'Test262 worker exited with status %d', [ExitCode]);
    end
    else if WIFSIGNALED(WaitStatus) then
      WorkerMessage := Format(
        'Test262 worker terminated by signal %d',
        [WTERMSIG(WaitStatus)])
    else
      WorkerMessage := 'Test262 worker terminated unexpectedly';
    if WorkerMessage <> '' then
    begin
      FResults[ResultIndex].Id := FCases[ResultIndex].Id;
      if not SlotTimedOut[Slot] then
        FResults[ResultIndex].Status := toWrapperInfra;
      FResults[ResultIndex].Message := WorkerMessage;
      FResults[ResultIndex].Diagnostic := WorkerMessage;
    end;
    DeleteFile(ResultPaths[Slot]);
    SlotPids[Slot] := 0;
    Dec(ActiveWorkers);
    Inc(CompletedCount);
    if FOptions.Verbose or
       (CompletedCount = FCases.Count) or
       ((CompletedCount mod 1024) = 0) then
    begin
      WriteLn('[', CompletedCount, '/', FCases.Count, ']');
      Flush(Output);
    end;
  end;
  for Slot := 0 to High(ResultPaths) do
    DeleteFile(ResultPaths[Slot]);
  {$ELSE}
  Pool := TGocciaThreadPool.Create(FOptions.Jobs);
  try
    Pool.MaxBytes := FOptions.MaxMemoryBytes;
    Pool.ResetRuntimeBetweenItems := True;
    Pool.RunAll(FSelectedPaths, WorkerProc, nil,
      FOptions.TimeoutMs * 2 + 1000);
    for I := 0 to High(Pool.Results) do
      if (not Pool.Results[I].Success) and
         (FResults[I].Id = '') then
      begin
        FResults[I].Id := FCases[I].Id;
        if StartsStr('TIMEOUT', Pool.Results[I].ErrorMessage) then
          FResults[I].Status := toTimeout
        else
          FResults[I].Status := toWrapperInfra;
        FResults[I].Message := Pool.Results[I].ErrorMessage;
        FResults[I].Diagnostic := Pool.Results[I].ErrorMessage;
      end;
  finally
    Pool.Free;
  end;
  {$ENDIF}
  FDurationNanoseconds := GetNanoseconds - StartNanoseconds;
end;

function CategoryForId(const AId: string): string;
var
  SeparatorPosition: SizeInt;
begin
  SeparatorPosition := Pos('/', AId);
  if SeparatorPosition > 0 then
    Result := Copy(AId, 1, SeparatorPosition - 1)
  else
    Result := AId;
end;

function CountOutcome(const AResults: array of TTest262Result;
  const AOutcome: TTest262Outcome): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(AResults) do
    if AResults[I].Status = AOutcome then
      Inc(Result);
end;

function PercentageText(const ANumerator, ADenominator: Integer): string;
begin
  if ADenominator = 0 then
    Exit('0.0%');
  Result := FormatFloat('0.0',
    ANumerator * 100.0 / ADenominator) + '%';
end;

procedure TTest262App.PrintSummary;
var
  Failed: Integer;
  I: Integer;
  Infrastructure: Integer;
  Passed: Integer;
  TimedOut: Integer;
begin
  Passed := CountOutcome(FResults, toPass);
  Failed := CountOutcome(FResults, toFail);
  Infrastructure := CountOutcome(FResults, toWrapperInfra);
  TimedOut := CountOutcome(FResults, toTimeout);
  WriteLn('');
  WriteLn('test262 conformance');
  WriteLn('  Discovered:       ', FTotalDiscovered);
  WriteLn('  Run:              ', Length(FResults));
  WriteLn('  Passed:           ', Passed, ' (',
    PercentageText(Passed, Length(FResults)), ')');
  WriteLn('  Failed:           ', Failed);
  WriteLn('  Wrapper infra:    ', Infrastructure);
  WriteLn('  Timeouts:         ', TimedOut);
  WriteLn('  Duration:         ',
    FormatFloat('0.0', FDurationNanoseconds / 1000000000), 's');
  for I := 0 to High(FResults) do
    if FOptions.Verbose or (FResults[I].Status <> toPass) then
      WriteLn('  ', OutcomeName(FResults[I].Status), ' ',
        FResults[I].Id,
        IfThen(FResults[I].Message <> '', ': ' + FResults[I].Message, ''));
end;

function JSONString(const AValue: string): string;
begin
  Result := QuoteJSONString(AValue);
end;

procedure BuildCategoryCounts(const AResults: array of TTest262Result;
  const ACategories: TStrings; out ARun, APassed, AFailed,
  AInfrastructure, ATimeouts: TIntegerArray);
var
  Category: string;
  CategoryIndex: Integer;
  I: Integer;
begin
  SetLength(ARun, ACategories.Count);
  SetLength(APassed, ACategories.Count);
  SetLength(AFailed, ACategories.Count);
  SetLength(AInfrastructure, ACategories.Count);
  SetLength(ATimeouts, ACategories.Count);
  for I := 0 to High(AResults) do
  begin
    Category := CategoryForId(AResults[I].Id);
    CategoryIndex := ACategories.IndexOf(Category);
    if CategoryIndex < 0 then
      Continue;
    Inc(ARun[CategoryIndex]);
    case AResults[I].Status of
      toPass: Inc(APassed[CategoryIndex]);
      toFail: Inc(AFailed[CategoryIndex]);
      toWrapperInfra: Inc(AInfrastructure[CategoryIndex]);
      toTimeout: Inc(ATimeouts[CategoryIndex]);
    end;
  end;
end;

procedure TTest262App.WriteReport;
var
  Categories: TStringList;
  CategoryFailed: TIntegerArray;
  CategoryInfrastructure: TIntegerArray;
  CategoryPassed: TIntegerArray;
  CategoryRun: TIntegerArray;
  CategoryTimeouts: TIntegerArray;
  DurationSeconds: Double;
  I: Integer;
  Lines: TStringList;
  Test262SHA: string;
begin
  if FOptions.OutputPath = '' then
    Exit;
  ForceDirectories(ExtractFileDir(FOptions.OutputPath));
  Categories := TStringList.Create;
  Lines := TStringList.Create;
  try
    Categories.StrictDelimiter := True;
    Categories.Delimiter := ',';
    Categories.DelimitedText := FOptions.Categories;
    for I := 0 to Categories.Count - 1 do
      Categories[I] := Trim(Categories[I]);
    BuildCategoryCounts(FResults, Categories, CategoryRun, CategoryPassed,
      CategoryFailed, CategoryInfrastructure, CategoryTimeouts);
    DurationSeconds := FDurationNanoseconds / 1000000000;

    Lines.Add('{');
    Lines.Add('  "summary": {');
    Lines.Add('    "totalDiscovered": ' + IntToStr(FTotalDiscovered) + ',');
    Lines.Add('    "totalRun": ' + IntToStr(Length(FResults)) + ',');
    Lines.Add('    "passed": ' +
      IntToStr(CountOutcome(FResults, toPass)) + ',');
    Lines.Add('    "failed": ' +
      IntToStr(CountOutcome(FResults, toFail)) + ',');
    Lines.Add('    "wrapperInfraFailures": ' +
      IntToStr(CountOutcome(FResults, toWrapperInfra)) + ',');
    Lines.Add('    "timeouts": ' +
      IntToStr(CountOutcome(FResults, toTimeout)) + ',');
    Lines.Add('    "durationSeconds": ' +
      Format('%.3f', [DurationSeconds], InvariantFormatSettings) + ',');
    Lines.Add('    "byCategory": [');
    for I := 0 to Categories.Count - 1 do
    begin
      Lines.Add('      {');
      Lines.Add('        "category": ' + JSONString(Categories[I]) + ',');
      Lines.Add('        "run": ' + IntToStr(CategoryRun[I]) + ',');
      Lines.Add('        "passed": ' + IntToStr(CategoryPassed[I]) + ',');
      Lines.Add('        "failed": ' + IntToStr(CategoryFailed[I]) + ',');
      Lines.Add('        "wrapperInfra": ' +
        IntToStr(CategoryInfrastructure[I]) + ',');
      Lines.Add('        "timeouts": ' +
        IntToStr(CategoryTimeouts[I]));
      if I < Categories.Count - 1 then
        Lines.Add('      },')
      else
        Lines.Add('      }');
    end;
    Lines.Add('    ]');
    Lines.Add('  },');
    Lines.Add('  "results": [');
    for I := 0 to High(FResults) do
    begin
      Lines.Add('    {');
      Lines.Add('      "id": ' + JSONString(FResults[I].Id) + ',');
      Lines.Add('      "status": ' +
        JSONString(OutcomeName(FResults[I].Status)) + ',');
      Lines.Add('      "durationMs": ' +
        IntToStr(FResults[I].DurationMs) + ',');
      Lines.Add('      "message": ' + JSONString(FResults[I].Message));
      if FResults[I].Diagnostic <> '' then
        Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + ',';
      if FResults[I].Diagnostic <> '' then
        Lines.Add('      "diagnostic": ' +
          JSONString(FResults[I].Diagnostic));
      if FResults[I].ProfilePath <> '' then
      begin
        Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + ',';
        Lines.Add('      "profilePath": ' +
          JSONString(FResults[I].ProfilePath) + ',');
        if FResults[I].ProfileMissing then
          Lines.Add('      "profileMissing": true')
        else
          Lines.Add('      "profileMissing": false');
      end;
      if I < High(FResults) then
        Lines.Add('    },')
      else
        Lines.Add('    }');
    end;
    Lines.Add('  ],');
    Lines.Add('  "run": {');
    Lines.Add('    "suiteDir": ' + JSONString(FOptions.SuiteDir) + ',');
    Test262SHA := GetEnvironmentVariable('TEST262_SUITE_SHA');
    if Test262SHA = '' then
      Lines.Add('    "test262Sha": null,')
    else
      Lines.Add('    "test262Sha": ' + JSONString(Test262SHA) + ',');
    if FOptions.Mode = t262emBytecode then
      Lines.Add('    "mode": "bytecode",')
    else
      Lines.Add('    "mode": "interpreted",');
    Lines.Add('    "categories": [');
    for I := 0 to Categories.Count - 1 do
    begin
      if I < Categories.Count - 1 then
        Lines.Add('      ' + JSONString(Categories[I]) + ',')
      else
        Lines.Add('      ' + JSONString(Categories[I]));
    end;
    Lines.Add('    ],');
    Lines.Add('    "jobs": ' + IntToStr(FOptions.Jobs) + ',');
    Lines.Add('    "timeoutMs": ' + IntToStr(FOptions.TimeoutMs) + ',');
    Lines.Add('    "maxMemoryBytes": ' +
      IntToStr(FOptions.MaxMemoryBytes));
    Lines.Add('  }' + IfThen(FOptions.ShardCount > 0, ',', ''));
    if FOptions.ShardCount > 0 then
    begin
      Lines.Add('  "shard": {');
      Lines.Add('    "index": ' + IntToStr(FOptions.ShardIndex) + ',');
      Lines.Add('    "count": ' + IntToStr(FOptions.ShardCount));
      Lines.Add('  }');
    end;
    Lines.Add('}');
    Lines.SaveToFile(FOptions.OutputPath);
  finally
    Lines.Free;
    Categories.Free;
  end;
end;

function TTest262App.FailureCount: Integer;
begin
  Result := CountOutcome(FResults, toFail) +
    CountOutcome(FResults, toWrapperInfra) +
    CountOutcome(FResults, toTimeout);
end;

function TTest262App.RunHostEval: Integer;
var
  Engine: TGocciaEngine;
  EngineOptions: TGocciaTest262EngineOptions;
  Executor: TGocciaExecutor;
  Host: TGocciaTest262Host;
  PrintHost: TTest262PrintHost;
  Source: TStringList;
begin
  Result := 0;
  Source := ReadSourceFromText(Input);
  Executor := nil;
  Engine := nil;
  Host := nil;
  PrintHost := nil;
  try
    EngineOptions := Default(TGocciaTest262EngineOptions);
    EngineOptions.Compatibility := [cfASI, cfVar, cfFunction,
      cfTraditionalFor, cfWhileLoops, cfLooseEquality, cfNonStrictMode,
      cfArgumentsObject, cfLabel, cfForIn];
    EngineOptions.UnsafeShadowRealm := True;
    EngineOptions.AgentCanSuspend := True;
    EngineOptions.Mode := FOptions.Mode;
    EngineOptions.SourceType := FOptions.HostEvalSourceType;
    EngineOptions.TimeoutMs := FOptions.TimeoutMs;
    EngineOptions.MaxMemoryBytes := FOptions.MaxMemoryBytes;
    EngineOptions.StackSize := DEFAULT_STACK_DEPTH;
    StartExecutionTimeout(FOptions.TimeoutMs);
    SetMaxStackDepth(DEFAULT_STACK_DEPTH);
    Executor := CreateTest262Executor(FOptions.Mode);
    Engine := TGocciaEngine.Create('<test262-host-eval>', Source, Executor);
    ConfigureTest262Engine(Engine, Executor, EngineOptions);
    if FOptions.HostEvalDeterministic then
      Engine.HostEnvironment.UseDeterministicProfile;
    Engine.WarningUnsupportedFeatures :=
      FOptions.HostEvalWarningUnsupportedFeatures;
    Host := TGocciaTest262Host.Create(EngineOptions);
    Host.ConfigureHostEnvironment(Engine.HostEnvironment);
    PrintHost := TTest262PrintHost.Create;
    Engine.RegisterGlobal('print',
      TGocciaNativeFunctionValue.CreateWithoutPrototype(
        PrintHost.PrintValue, 'print', -1));
    Host.Install(Engine);
    try
      Engine.Execute;
      Write(PrintHost.Text);
    except
      on E: EGocciaBytecodeThrow do
      begin
        WriteLn(ErrOutput, FormatThrowDetail(E.ThrownValue,
          '<test262-host-eval>', Source, False));
        Result := 1;
      end;
      on E: TGocciaThrowValue do
      begin
        WriteLn(ErrOutput, FormatThrowDetail(E.Value,
          '<test262-host-eval>', Source, False, E.Suggestion));
        Result := 1;
      end;
      on E: TGocciaError do
      begin
        WriteLn(ErrOutput, E.GetDetailedMessage(False));
        Result := 1;
      end;
    end;
  finally
    PrintHost.Free;
    Host.Free;
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

function TTest262App.Run: Integer;
begin
  ParseArguments;
  if FOptions.HostEval then
    Exit(RunHostEval);
  WriteLn('Suite:         ', FOptions.SuiteDir);
  WriteLn('Categories:    ', FOptions.Categories);
  if FOptions.Mode = t262emBytecode then
    WriteLn('Mode:          bytecode')
  else
    WriteLn('Mode:          interpreted');
  WriteLn('Jobs:          ', FOptions.Jobs);
  WriteLn('Timeout:       ', FOptions.TimeoutMs, 'ms');
  if FOptions.ShardCount > 0 then
    WriteLn('Shard:         ', FOptions.ShardIndex + 1, '/',
      FOptions.ShardCount);
  Discover;
  WriteLn('Discovered ', FTotalDiscovered, ' test files.');
  if FOptions.ShardCount > 0 then
    WriteLn('Selected ', FCases.Count, ' tests for this shard.');
  ExecuteAll;
  WriteReport;
  PrintSummary;
  if FailureCount > 0 then
    Result := 1
  else
    Result := 0;
end;

var
  App: TTest262App;
begin
  App := TTest262App.Create;
  try
    try
      ExitCode := App.Run;
    except
      on E: Exception do
      begin
        WriteLn(ErrOutput, 'Error: ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    App.Free;
  end;
end.
