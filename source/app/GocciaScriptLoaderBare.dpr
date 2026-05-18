program GocciaScriptLoaderBare;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Executor,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.ScriptLoader.Input,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TBarePrintHost = class
  public
    function Print(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareExecutionMode = (bemInterpreted, bemBytecode);

  TBareOptions = record
    ASI: Boolean;
    CompatVar: Boolean;
    CompatFunction: Boolean;
    CompatTraditionalFor: Boolean;
    CompatWhileLoops: Boolean;
    CompatLooseEquality: Boolean;
    CompatNonStrictMode: Boolean;
    StrictTypes: Boolean;
    UnsafeFunctionConstructor: Boolean;
    Print: Boolean;
    Mode: TBareExecutionMode;
    SourceType: TGocciaSourceType;
    FileName: string;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
  end;

const
  BARE_PRINT_GLOBAL_NAME = 'print';

function TBarePrintHost.Print(const AArgs: TGocciaArgumentsCollection;
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

  WriteLn(Line);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure PrintUsage;
begin
  WriteLn('Usage: GocciaScriptLoaderBare [file|-] [options]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --asi                         Enable automatic semicolon insertion');
  WriteLn('  --compat-var                  Enable var declarations');
  WriteLn('  --compat-function             Enable function declarations/expressions');
  WriteLn('  --compat-traditional-for-loop Enable traditional C-style for(;;) loops');
  WriteLn('  --compat-while-loops          Enable while and do...while loops');
  WriteLn('  --compat-loose-equality       Enable loose equality (== and !=)');
  WriteLn('  --compat-non-strict-mode      Enable non-strict-mode compatibility');
  WriteLn('  --strict-types                Enforce type annotations at runtime');
  WriteLn('  --mode=interpreted|bytecode   Execution mode (default: interpreted)');
  WriteLn('  --source-type=script|module   Load entry as a script or module');
  WriteLn('  --unsafe-function-constructor Enable dynamic Function constructor');
  WriteLn('  --print                       Print the script''s last value (incl. undefined)');
  WriteLn('  --timeout=MS                  Per-file cooperative timeout in milliseconds');
  WriteLn('  --max-memory=BYTES            GC heap byte limit (RangeError on exceed)');
  WriteLn('  --max-instructions=N          Maximum bytecode steps before aborting');
  WriteLn('  --help                        Show this help');
end;

procedure ParseMode(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'interpreted' then
    AOptions.Mode := bemInterpreted
  else if AValue = 'bytecode' then
    AOptions.Mode := bemBytecode
  else
    raise Exception.Create('Invalid --mode value: ' + AValue);
end;

procedure ParseSourceType(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'script' then
    AOptions.SourceType := stScript
  else if AValue = 'module' then
    AOptions.SourceType := stModule
  else
    raise Exception.Create('Invalid --source-type value: ' + AValue);
end;

procedure ParseTimeout(const AValue: string; var AOptions: TBareOptions);
var
  Parsed: Integer;
begin
  if not TryStrToInt(AValue, Parsed) then
    raise Exception.Create('Invalid --timeout value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--timeout must be 0 or greater');
  AOptions.TimeoutMs := Parsed;
end;

procedure ParseMaxMemory(const AValue: string; var AOptions: TBareOptions);
var
  Parsed: Int64;
begin
  if not TryStrToInt64(AValue, Parsed) then
    raise Exception.Create('Invalid --max-memory value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--max-memory must be 0 or greater');
  AOptions.MaxMemoryBytes := Parsed;
end;

procedure ParseMaxInstructions(const AValue: string;
  var AOptions: TBareOptions);
var
  Parsed: Int64;
begin
  if not TryStrToInt64(AValue, Parsed) then
    raise Exception.Create('Invalid --max-instructions value: ' + AValue);
  if Parsed < 0 then
    raise Exception.Create('--max-instructions must be 0 or greater');
  AOptions.MaxInstructions := Parsed;
end;

function ParseOptions: TBareOptions;
var
  I: Integer;
  Arg: string;
begin
  Result.ASI := False;
  Result.CompatVar := False;
  Result.CompatFunction := False;
  Result.CompatTraditionalFor := False;
  Result.CompatWhileLoops := False;
  Result.CompatLooseEquality := False;
  Result.CompatNonStrictMode := False;
  Result.StrictTypes := False;
  Result.UnsafeFunctionConstructor := False;
  Result.Print := False;
  Result.Mode := bemInterpreted;
  Result.SourceType := stScript;
  Result.FileName := STDIN_PATH_MARKER;
  Result.TimeoutMs := 0;
  Result.MaxMemoryBytes := 0;
  Result.MaxInstructions := 0;

  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if Arg = '--help' then
    begin
      PrintUsage;
      Halt(0);
    end
    else if Arg = '--asi' then
      Result.ASI := True
    else if Arg = '--compat-var' then
      Result.CompatVar := True
    else if Arg = '--compat-function' then
      Result.CompatFunction := True
    else if Arg = '--compat-traditional-for-loop' then
      Result.CompatTraditionalFor := True
    else if Arg = '--compat-while-loops' then
      Result.CompatWhileLoops := True
    else if Arg = '--compat-loose-equality' then
      Result.CompatLooseEquality := True
    else if Arg = '--compat-non-strict-mode' then
      Result.CompatNonStrictMode := True
    else if Arg = '--strict-types' then
      Result.StrictTypes := True
    else if Arg = '--unsafe-function-constructor' then
      Result.UnsafeFunctionConstructor := True
    else if Arg = '--print' then
      Result.Print := True
    else if Copy(Arg, 1, Length('--mode=')) = '--mode=' then
      ParseMode(Copy(Arg, Length('--mode=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--source-type=')) = '--source-type=' then
      ParseSourceType(Copy(Arg, Length('--source-type=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--timeout=')) = '--timeout=' then
      ParseTimeout(Copy(Arg, Length('--timeout=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--max-memory=')) = '--max-memory=' then
      ParseMaxMemory(Copy(Arg, Length('--max-memory=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--max-instructions=')) = '--max-instructions=' then
      ParseMaxInstructions(Copy(Arg, Length('--max-instructions=') + 1, MaxInt),
        Result)
    else if Copy(Arg, 1, 2) = '--' then
      raise Exception.Create('Unknown option: ' + Arg)
    else if Result.FileName = STDIN_PATH_MARKER then
      Result.FileName := Arg
    else
      raise Exception.Create('Unexpected argument: ' + Arg);
  end;
end;

function ReadBareSource(const AFileName: string): TStringList;
begin
  if IsStdinPath(AFileName) then
    Result := ReadSourceFromText(Input)
  else
    Result := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
end;

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AOptions: TBareOptions);
begin
  AEngine.ASIEnabled := AOptions.ASI;
  AEngine.VarEnabled := AOptions.CompatVar;
  AEngine.FunctionEnabled := AOptions.CompatFunction;
  AEngine.TraditionalForLoopsEnabled := AOptions.CompatTraditionalFor;
  AEngine.WhileLoopsEnabled := AOptions.CompatWhileLoops;
  AEngine.LooseEqualityEnabled := AOptions.CompatLooseEquality;
  AEngine.NonStrictModeEnabled := AOptions.CompatNonStrictMode;
  AEngine.StrictTypes := AOptions.StrictTypes;
  AEngine.SourceType := AOptions.SourceType;
  AEngine.FunctionConstructor.Enabled := AOptions.UnsafeFunctionConstructor;
end;

procedure RegisterBareGlobals(const AEngine: TGocciaEngine;
  const APrintHost: TBarePrintHost);
begin
  AEngine.RegisterGlobal(BARE_PRINT_GLOBAL_NAME,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(APrintHost.Print,
      BARE_PRINT_GLOBAL_NAME, -1));
  AEngine.RefreshGlobalThis;
end;

procedure PrintResult(const AResult: TGocciaValue; const APrint: Boolean);
begin
  { Mirrors `node -p` / `bun --print` / `deno eval -p`: silent by default,
    prints the bare value (incl. `undefined`) only when --print is set. }
  if not APrint then
    Exit;
  if not Assigned(AResult) then
    Exit;
  WriteLn(AResult.ToStringLiteral.Value);
end;

function CreateExecutorForMode(
  const AMode: TBareExecutionMode): TGocciaExecutor;
begin
  case AMode of
    bemInterpreted: Result := TGocciaInterpreterExecutor.Create;
    bemBytecode: Result := TGocciaBytecodeExecutor.Create;
  end;
end;

function RunBare(const AOptions: TBareOptions): Integer;
var
  DisplayName: string;
  Engine: TGocciaEngine;
  Executor: TGocciaExecutor;
  GC: TGarbageCollector;
  PrintHost: TBarePrintHost;
  ScriptResult: TGocciaScriptResult;
  Source: TStringList;
begin
  Result := 0;
  Source := ReadBareSource(AOptions.FileName);
  try
    if IsStdinPath(AOptions.FileName) then
      DisplayName := STDIN_FILE_NAME
    else
      DisplayName := AOptions.FileName;

    { Engine-side bounds.  StartExecutionTimeout / StartInstructionLimit
      are threadvar-backed and order-independent vs. engine creation, so
      they stay here.  --max-memory must be applied AFTER Engine.Create —
      the engine constructor calls TGarbageCollector.Initialize, which
      lazy-creates the GC singleton with FMaxBytes := FSuggestedMaxBytes
      and would otherwise overwrite any pre-engine value. }
    StartExecutionTimeout(AOptions.TimeoutMs);
    StartInstructionLimit(AOptions.MaxInstructions);

    PrintHost := TBarePrintHost.Create;
    try
      Executor := CreateExecutorForMode(AOptions.Mode);
      try
        Engine := TGocciaEngine.Create(DisplayName, Source, Executor);
        try
          ConfigureEngine(Engine, AOptions);

          GC := TGarbageCollector.Instance;
          if Assigned(GC) and (AOptions.MaxMemoryBytes > 0) then
            GC.MaxBytes := AOptions.MaxMemoryBytes;

          RegisterBareGlobals(Engine, PrintHost);
          try
            ScriptResult := Engine.Execute;
            PrintResult(ScriptResult.Result, AOptions.Print);
          except
            on E: TGocciaThrowValue do
            begin
              WriteLn(StdErr, FormatThrowDetail(E.Value, DisplayName, Source,
                IsColorTerminal, E.Suggestion));
              Result := 1;
            end;
          end;
        finally
          Engine.Free;
        end;
      finally
        Executor.Free;
      end;
    finally
      PrintHost.Free;
    end;
  finally
    Source.Free;
  end;
end;

var
  Options: TBareOptions;
begin
  try
    Options := ParseOptions;
    ExitCode := RunBare(Options);
  except
    on E: TGocciaError do
    begin
      WriteLn(StdErr, E.GetDetailedMessage(IsColorTerminal));
      ExitCode := 1;
    end;
    on E: Exception do
    begin
      WriteLn(StdErr, 'Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
