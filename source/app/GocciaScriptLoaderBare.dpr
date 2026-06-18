program GocciaScriptLoaderBare;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  StrUtils,
  SysUtils,

  TextSemantics,

  Goccia.Arguments.Collection,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.CLI.Options,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.Executor,
  Goccia.Executor.Bytecode,
  Goccia.Executor.Interpreter,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.Modules,
  Goccia.Modules.ContentProvider,
  Goccia.Profiler,
  Goccia.Profiler.Report,
  Goccia.Scope,
  Goccia.Scope.Redeclaration,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Timeout,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

type
  TBarePrintHost = class
  public
    function Print(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareExecutionMode = (bemInterpreted, bemBytecode);

  TBareOptions = record
    Compatibility: TGocciaCompatibilityFlags;
    StrictTypes: Boolean;
    UnsafeFunctionConstructor: Boolean;
    Test262Host: Boolean;
    Print: Boolean;
    Mode: TBareExecutionMode;
    SourceType: TGocciaSourceType;
    SourceTypeExplicit: Boolean;
    FileName: string;
    SourceName: string;
    TimeoutMs: Integer;
    MaxMemoryBytes: Int64;
    MaxInstructions: Int64;
    ProfileModePresent: Boolean;
    ProfileMode: Goccia.CLI.Options.TGocciaProfileMode;
    ProfileOutputPath: string;
  end;

  TBareTest262Realm = class;
  TBareTest262Host = class;

  TBareTest262EvalHost = class
  private
    FEngine: TGocciaEngine;
  public
    constructor Create(const AEngine: TGocciaEngine);
    function Eval(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareTest262Realm = class
  private
    FEngine: TGocciaEngine;
    FExecutor: TGocciaExecutor;
    FEvalHost: TBareTest262EvalHost;
    FTest262Host: TBareTest262Host;
    FSource: TStringList;
  public
    constructor Create(const AOptions: TBareOptions);
    destructor Destroy; override;
    function EvalScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function GlobalObject: TGocciaObjectValue;
    property Engine: TGocciaEngine read FEngine;
  end;

  TBareTest262Host = class
  private
    FOptions: TBareOptions;
    FRealms: TObjectList<TBareTest262Realm>;
  public
    constructor Create(const AOptions: TBareOptions);
    destructor Destroy; override;
    function CreateRealm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TBareTest262ModuleContentProvider = class(TGocciaFileSystemModuleContentProvider)
  private
    function BuildHarnessPrelude(const APath: string;
      const ASource: UTF8String): UTF8String;
    function FindSuiteRoot(const APath: string): string;
    function HarnessFilePath(const ASuiteRoot, AName: string): string;
    function ShouldPrependHarness(const APath: string): Boolean;
  public
    function LoadContent(const APath: string): TGocciaModuleContent; override;
  end;

const
  BARE_PRINT_GLOBAL_NAME = 'print';

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor; const AOptions: TBareOptions); forward;
function CreateExecutorForMode(
  const AMode: TBareExecutionMode): TGocciaExecutor; forward;

procedure InstallTest262EvalGlobal(const AEngine: TGocciaEngine;
  const AEvalHost: TBareTest262EvalHost);
var
  EvalFunction: TGocciaNativeFunctionValue;
  GlobalObject: TGocciaObjectValue;
begin
  EvalFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    AEvalHost.Eval, 'eval', 1);
  EvalFunction.DirectEvalHost := True;
  GlobalObject := TGocciaObjectValue(AEngine.Realm.GlobalObject);
  GlobalObject.DefineProperty('eval',
    TGocciaPropertyDescriptorData.Create(EvalFunction,
      [pfWritable, pfConfigurable]));
end;

procedure InstallTest262HostGlobals(const AEngine: TGocciaEngine;
  const ATest262Host: TBareTest262Host;
  const ATest262EvalHost: TBareTest262EvalHost);
var
  Test262Obj: TGocciaObjectValue;
begin
  AEngine.GocciaGlobal.AssignProperty('test262Host',
    TGocciaBooleanLiteralValue.TrueValue);
  Test262Obj := TGocciaObjectValue.Create;
  Test262Obj.AssignProperty('createRealm',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ATest262Host.CreateRealm, 'createRealm', 0));
  Test262Obj.AssignProperty('evalScript',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ATest262EvalHost.EvalScript, 'evalScript', 1));
  Test262Obj.AssignProperty('abstractModuleSourcePrototype',
    TGocciaModuleSourceValue.SharedPrototype);
  AEngine.GocciaGlobal.AssignProperty('test262', Test262Obj);
end;

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

function HasEvalTopLevelUsingDeclaration(
  const AProgram: TGocciaProgram): Boolean;
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    if AProgram.Body[I] is TGocciaUsingDeclaration then
      Exit(True);
  Result := False;
end;

constructor TBareTest262EvalHost.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TBareTest262EvalHost.Eval(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  EvalOptions: TGocciaSourcePipelineOptions;
  EvalSource: TStringList;
  PipelineResult: TGocciaSourcePipelineResult;
  RealmScope: TGocciaExecutionContextScope;
  EvalScope: TGocciaScope;
  VarScope: TGocciaScope;
  EvalContext: TGocciaEvaluationContext;
  SourceValue: TGocciaValue;
  SourceText: string;
  StrictEval: Boolean;
begin
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  SourceValue := AArgs.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
    Exit(SourceValue);

  SourceText := TGocciaStringLiteralValue(SourceValue).Value;
  EvalSource := TStringList.Create;
  try
    EvalSource.Text := SourceText;
    EvalOptions := TGocciaSourcePipeline.DefaultOptions;
    EvalOptions.Preprocessors := FEngine.Preprocessors;
    EvalOptions.Compatibility := FEngine.Compatibility;
    EvalOptions.SourceType := stScript;

    ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(EvalOptions);
    try
      RealmScope := FEngine.ActivateRealmExecutionContext;
      try
        PipelineResult := TGocciaSourcePipeline.Parse(EvalSource,
          '<test262-realm-eval>', EvalOptions);
        try
          if HasEvalTopLevelUsingDeclaration(PipelineResult.ProgramNode) then
            ThrowSyntaxError(
              'Using declarations are not allowed at the top level of eval');
          StrictEval := HasUseStrictDirective(PipelineResult.ProgramNode);
          if StrictEval then
            EvalScope := FEngine.Interpreter.GlobalScope.CreateChild(skFunction,
              'StrictTest262Eval')
          else
            EvalScope := FEngine.Interpreter.GlobalScope.CreateChild(skBlock,
              'Test262Eval');
          EvalScope.ThisValue := FEngine.Interpreter.GlobalScope.ThisValue;
          if Assigned(TGarbageCollector.Instance) then
            TGarbageCollector.Instance.AddTempRoot(EvalScope);
          try
            EvalContext := FEngine.Interpreter.CreateEvaluationContext;
            EvalContext.Scope := EvalScope;
            EvalContext.CurrentFilePath := '<test262-realm-eval>';
            EvalContext.NonStrictMode := not StrictEval;
            if StrictEval then
              VarScope := EvalScope
            else
              VarScope := FEngine.Interpreter.GlobalScope;
            Result := EvaluateEvalProgram(PipelineResult.ProgramNode,
              EvalContext, VarScope, EvalScope, StrictEval, False, nil, False,
              False, False);
          finally
            if Assigned(TGarbageCollector.Instance) then
              TGarbageCollector.Instance.RemoveTempRoot(EvalScope);
          end;
        finally
          PipelineResult.Free;
        end;
      finally
        RealmScope.Free;
      end;
    finally
      ActiveOptionsScope.Free;
    end;
  finally
    EvalSource.Free;
  end;
end;

function TBareTest262EvalHost.EvalScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ActiveOptionsScope: TGocciaSourcePipelineOptionsScope;
  ScriptOptions: TGocciaSourcePipelineOptions;
  ScriptSource: TStringList;
  PipelineResult: TGocciaSourcePipelineResult;
  RealmScope: TGocciaExecutionContextScope;
  SourceValue: TGocciaValue;
  SourceText: string;
begin
  if AArgs.Length = 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);

  SourceValue := AArgs.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
    Exit(SourceValue);

  SourceText := TGocciaStringLiteralValue(SourceValue).Value;
  ScriptSource := TStringList.Create;
  try
    ScriptSource.Text := SourceText;
    ScriptOptions := TGocciaSourcePipeline.DefaultOptions;
    ScriptOptions.Preprocessors := FEngine.Preprocessors;
    ScriptOptions.Compatibility := FEngine.Compatibility;
    ScriptOptions.SourceType := stScript;

    ActiveOptionsScope := TGocciaSourcePipeline.ActivateOptions(ScriptOptions);
    try
      RealmScope := FEngine.ActivateRealmExecutionContext;
      try
        PipelineResult := TGocciaSourcePipeline.Parse(ScriptSource,
          '<test262-eval-script>', ScriptOptions);
        try
          CheckTopLevelRedeclarations(PipelineResult.ProgramNode,
            FEngine.Interpreter.GlobalScope, '<test262-eval-script>');
          Result := FEngine.ExecuteProgram(PipelineResult.ProgramNode);
        finally
          PipelineResult.Free;
        end;
      finally
        RealmScope.Free;
      end;
    finally
      ActiveOptionsScope.Free;
    end;
  finally
    ScriptSource.Free;
  end;
end;

constructor TBareTest262Realm.Create(const AOptions: TBareOptions);
var
  ChildOptions: TBareOptions;
begin
  inherited Create;
  FSource := TStringList.Create;
  FExecutor := CreateExecutorForMode(AOptions.Mode);
  FEngine := TGocciaEngine.Create('<test262-realm>', FSource, FExecutor);
  ChildOptions := AOptions;
  ChildOptions.SourceType := stScript;
  ChildOptions.Test262Host := True;
  ConfigureEngine(FEngine, FExecutor, ChildOptions);

  FEvalHost := TBareTest262EvalHost.Create(FEngine);
  FTest262Host := TBareTest262Host.Create(ChildOptions);
  FEngine.RefreshGlobalThis;
  InstallTest262HostGlobals(FEngine, FTest262Host, FEvalHost);
  InstallTest262EvalGlobal(FEngine, FEvalHost);
  FEngine.SuspendRealmExecutionContext;
end;

destructor TBareTest262Realm.Destroy;
var
  RealmScope: TGocciaExecutionContextScope;
begin
  if Assigned(FEngine) then
  begin
    RealmScope := FEngine.ActivateRealmExecutionContext;
    try
      FEngine.Free;
    finally
      RealmScope.Free;
    end;
  end;
  FTest262Host.Free;
  FEvalHost.Free;
  FExecutor.Free;
  FSource.Free;
  inherited;
end;

function TBareTest262Realm.GlobalObject: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue(FEngine.Realm.GlobalObject);
end;

function TBareTest262Realm.EvalScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FEvalHost.EvalScript(AArgs, AThisValue);
end;

constructor TBareTest262Host.Create(const AOptions: TBareOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FRealms := TObjectList<TBareTest262Realm>.Create(True);
end;

destructor TBareTest262Host.Destroy;
begin
  FRealms.Free;
  inherited;
end;

function TBareTest262Host.CreateRealm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Realm: TBareTest262Realm;
  RealmRecord: TGocciaObjectValue;
begin
  Realm := TBareTest262Realm.Create(FOptions);
  FRealms.Add(Realm);

  RealmRecord := TGocciaObjectValue.Create;
  RealmRecord.AssignProperty('global', Realm.GlobalObject);
  RealmRecord.AssignProperty('evalScript',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Realm.EvalScript, 'evalScript', 1));
  RealmRecord.AssignProperty('createRealm',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      CreateRealm, 'createRealm', 0));
  Result := RealmRecord;
end;

function NormalizeTest262ListItem(const AValue: string): string;
begin
  Result := Trim(AValue);
  if (Result <> '') and (Result[Length(Result)] = ',') then
    Delete(Result, Length(Result), 1);
  Result := Trim(Result);
  if (Length(Result) >= 2) and
     (((Result[1] = '''') and (Result[Length(Result)] = '''')) or
      ((Result[1] = '"') and (Result[Length(Result)] = '"'))) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure AddTest262ListItem(const AItems: TStrings; const AValue: string);
var
  Item: string;
begin
  Item := NormalizeTest262ListItem(AValue);
  if (Item <> '') and (AItems.IndexOf(Item) < 0) then
    AItems.Add(Item);
end;

procedure AddTest262InlineList(const AItems: TStrings; const AValue: string);
var
  I: Integer;
  Item: string;
  ListText: string;
  Parts: TStringArray;
begin
  ListText := Trim(AValue);
  if ListText = '' then
    Exit;

  if (ListText[1] = '[') then
  begin
    Delete(ListText, 1, 1);
    I := Pos(']', ListText);
    if I > 0 then
      ListText := Copy(ListText, 1, I - 1);
  end;

  Parts := ListText.Split([',']);
  for Item in Parts do
    AddTest262ListItem(AItems, Item);
end;

procedure ReadTest262Frontmatter(const ASource: UTF8String;
  const AIncludes: TStrings; out ARaw: Boolean);
var
  Block: string;
  ClosePos: SizeInt;
  ColonPos: SizeInt;
  Key: string;
  Line: string;
  Lines: TStringList;
  OpenPos: SizeInt;
  Trimmed: string;
  Value: string;
  InFlags: Boolean;
  InIncludes: Boolean;
begin
  ARaw := False;
  OpenPos := Pos('/*---', string(ASource));
  if OpenPos <= 0 then
    Exit;
  ClosePos := PosEx('---*/', string(ASource), OpenPos + Length('/*---'));
  if ClosePos <= OpenPos then
    Exit;

  Block := Copy(string(ASource), OpenPos + Length('/*---'),
    ClosePos - OpenPos - Length('/*---'));
  Lines := TStringList.Create;
  try
    Lines.Text := Block;
    InFlags := False;
    InIncludes := False;
    for Line in Lines do
    begin
      Trimmed := Trim(Line);
      if Trimmed = '' then
        Continue;

      if (Line[1] <> ' ') and (Line[1] <> #9) then
      begin
        ColonPos := Pos(':', Line);
        if ColonPos <= 0 then
        begin
          InFlags := False;
          InIncludes := False;
          Continue;
        end;

        Key := Trim(Copy(Line, 1, ColonPos - 1));
        Value := Trim(Copy(Line, ColonPos + 1, MaxInt));
        InFlags := Key = 'flags';
        InIncludes := Key = 'includes';
        if InFlags then
        begin
          AddTest262InlineList(AIncludes, '');
          ARaw := Pos('raw', Value) > 0;
        end
        else if InIncludes then
          AddTest262InlineList(AIncludes, Value);
        Continue;
      end;

      if (InIncludes or InFlags) and StartsStr('-', Trimmed) then
      begin
        Value := Trim(Copy(Trimmed, 2, MaxInt));
        if InFlags then
          ARaw := ARaw or (NormalizeTest262ListItem(Value) = 'raw')
        else
          AddTest262ListItem(AIncludes, Value);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TBareTest262ModuleContentProvider.FindSuiteRoot(
  const APath: string): string;
var
  Candidate: string;
  Dir: string;
  ParentDir: string;
begin
  Result := '';
  Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(APath)));
  while Dir <> '' do
  begin
    Candidate := IncludeTrailingPathDelimiter(Dir) + 'harness' + PathDelim +
      'assert.js';
    if FileExists(Candidate) and
       DirectoryExists(IncludeTrailingPathDelimiter(Dir) + 'test') then
      Exit(Dir);

    ParentDir := ExtractFileDir(Dir);
    if (ParentDir = '') or (ParentDir = Dir) then
      Break;
    Dir := ParentDir;
  end;
end;

function TBareTest262ModuleContentProvider.HarnessFilePath(
  const ASuiteRoot, AName: string): string;
var
  BundledPath: string;
begin
  if (AName = '$262.js') or (AName = 'goccia-global-shim.js') then
  begin
    BundledPath := ExpandFileName('scripts' + PathDelim +
      'test262_harness' + PathDelim + AName);
    if FileExists(BundledPath) then
      Exit(BundledPath);
  end;

  Result := IncludeTrailingPathDelimiter(ASuiteRoot) + 'harness' + PathDelim +
    AName;
end;

function TBareTest262ModuleContentProvider.ShouldPrependHarness(
  const APath: string): Boolean;
var
  AbsPath: string;
  SuiteRoot: string;
  TestRoot: string;
begin
  if LowerCase(ExtractFileExt(APath)) <> '.js' then
    Exit(False);
  if EndsStr('_FIXTURE.js', ExtractFileName(APath)) then
    Exit(False);

  SuiteRoot := FindSuiteRoot(APath);
  if SuiteRoot = '' then
    Exit(False);

  AbsPath := ExpandFileName(APath);
  TestRoot := IncludeTrailingPathDelimiter(SuiteRoot) + 'test' + PathDelim;
  Result := StartsStr(TestRoot, AbsPath);
end;

function TBareTest262ModuleContentProvider.BuildHarnessPrelude(
  const APath: string; const ASource: UTF8String): UTF8String;
var
  I: Integer;
  IncludeName: string;
  Includes: TStringList;
  Raw: Boolean;
  SuiteRoot: string;
begin
  Result := '';
  SuiteRoot := FindSuiteRoot(APath);
  if SuiteRoot = '' then
    Exit;

  Includes := TStringList.Create;
  try
    Includes.Add('$262.js');
    Includes.Add('sta.js');
    Includes.Add('assert.js');
    ReadTest262Frontmatter(ASource, Includes, Raw);
    if Raw then
      Exit;

    for I := 0 to Includes.Count - 1 do
    begin
      IncludeName := Includes[I];
      if Result <> '' then
        Result := Result + LineEnding;
      Result := Result + ReadUTF8FileText(
        HarnessFilePath(SuiteRoot, IncludeName));
    end;
    if Result <> '' then
      Result := Result + LineEnding + ReadUTF8FileText(
        HarnessFilePath(SuiteRoot, 'goccia-global-shim.js'));
  finally
    Includes.Free;
  end;
end;

function TBareTest262ModuleContentProvider.LoadContent(
  const APath: string): TGocciaModuleContent;
var
  HarnessPrelude: UTF8String;
  OriginalContent: TGocciaModuleContent;
begin
  OriginalContent := inherited LoadContent(APath);
  if not ShouldPrependHarness(APath) then
    Exit(OriginalContent);

  try
    HarnessPrelude := BuildHarnessPrelude(APath, OriginalContent.Text);
    if HarnessPrelude = '' then
      Exit(OriginalContent);

    Result := TGocciaModuleContent.Create(HarnessPrelude + LineEnding +
      OriginalContent.Text, OriginalContent.LastModified);
  except
    OriginalContent.Free;
    raise;
  end;
  OriginalContent.Free;
end;

procedure PrintUsage;
var
  Flag: TGocciaCompatibility;
  Descriptor: TGocciaCompatibilityFlagDescriptor;
begin
  WriteLn('Usage: GocciaScriptLoaderBare [file|-] [options]');
  WriteLn('');
  WriteLn('Options:');
  for Flag := Low(TGocciaCompatibility) to High(TGocciaCompatibility) do
  begin
    Descriptor := CompatibilityFlagDescriptor(Flag);
    WriteLn(Format('  --%-28s %s', [Descriptor.OptionName,
      Descriptor.HelpText]));
  end;
  WriteLn('  --strict-types                Enforce type annotations at runtime');
  WriteLn('  --mode=interpreted|bytecode   Execution mode (default: interpreted)');
  WriteLn('  --source-type=script|module   Load entry as script source or module source (.mjs infers module)');
  WriteLn('  --source-name=PATH            Name stdin source as PATH for diagnostics and module resolution');
  WriteLn('  --unsafe-function-constructor Enable dynamic Function constructor');
  WriteLn('  --test262-host                Expose Goccia.test262 host hooks for test262');
  WriteLn('  --print                       Print the script''s last value (incl. undefined)');
  WriteLn('  --timeout=MS                  Per-file cooperative timeout in milliseconds');
  WriteLn('  --max-memory=BYTES            GC heap byte limit (RangeError on exceed)');
  WriteLn('  --max-instructions=N          Maximum bytecode steps before aborting');
  WriteLn('  --profile=opcodes|functions|all');
  WriteLn('                                Enable bytecode VM profiling (forces bytecode mode)');
  WriteLn('  --profile-output=PATH         Write profiler JSON to PATH');
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
  AOptions.SourceTypeExplicit := True;
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

procedure ParseProfileMode(const AValue: string; var AOptions: TBareOptions);
begin
  if AValue = 'opcodes' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmOpcodes
  else if AValue = 'functions' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmFunctions
  else if AValue = 'all' then
    AOptions.ProfileMode := Goccia.CLI.Options.pmAll
  else
    raise Exception.Create('Invalid --profile value: ' + AValue);
  AOptions.ProfileModePresent := True;
  AOptions.Mode := bemBytecode;
end;

procedure InitializeProfiler(const AOptions: TBareOptions);
begin
  if not AOptions.ProfileModePresent then
    Exit;

  TGocciaProfiler.Initialize;
  TGocciaProfiler.Instance.Enabled := True;
  case AOptions.ProfileMode of
    Goccia.CLI.Options.pmOpcodes:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes];
    Goccia.CLI.Options.pmFunctions:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmFunctions];
    Goccia.CLI.Options.pmAll:
      TGocciaProfiler.Instance.Mode := [Goccia.Profiler.pmOpcodes,
        Goccia.Profiler.pmFunctions];
  end;
end;

procedure WriteProfilerReport(const AOptions: TBareOptions);
begin
  if (not AOptions.ProfileModePresent) or
     (AOptions.ProfileOutputPath = '') or
     (not Assigned(TGocciaProfiler.Instance)) then
    Exit;

  WriteProfileJSON(TGocciaProfiler.Instance, AOptions.ProfileOutputPath);
end;

function ParseOptions: TBareOptions;
var
  I: Integer;
  Arg: string;
begin
  Result.Compatibility := [];
  Result.StrictTypes := False;
  Result.UnsafeFunctionConstructor := False;
  Result.Test262Host := False;
  Result.Print := False;
  Result.Mode := bemInterpreted;
  Result.SourceType := stScript;
  Result.SourceTypeExplicit := False;
  Result.FileName := STDIN_PATH_MARKER;
  Result.SourceName := '';
  Result.TimeoutMs := 0;
  Result.MaxMemoryBytes := 0;
  Result.MaxInstructions := 0;
  Result.ProfileModePresent := False;
  Result.ProfileMode := Goccia.CLI.Options.pmAll;
  Result.ProfileOutputPath := '';

  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if Arg = '--help' then
    begin
      PrintUsage;
      Halt(0);
    end
    else if TryApplyCompatibilityFlagArg(Arg, Result.Compatibility) then
    begin
      { handled by source compatibility flag registry }
    end
    else if Arg = '--strict-types' then
      Result.StrictTypes := True
    else if Arg = '--unsafe-function-constructor' then
      Result.UnsafeFunctionConstructor := True
    else if Arg = '--test262-host' then
      Result.Test262Host := True
    else if Arg = '--print' then
      Result.Print := True
    else if Copy(Arg, 1, Length('--mode=')) = '--mode=' then
      ParseMode(Copy(Arg, Length('--mode=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--source-type=')) = '--source-type=' then
      ParseSourceType(Copy(Arg, Length('--source-type=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--source-name=')) = '--source-name=' then
      Result.SourceName := Copy(Arg, Length('--source-name=') + 1, MaxInt)
    else if Copy(Arg, 1, Length('--timeout=')) = '--timeout=' then
      ParseTimeout(Copy(Arg, Length('--timeout=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--max-memory=')) = '--max-memory=' then
      ParseMaxMemory(Copy(Arg, Length('--max-memory=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--max-instructions=')) = '--max-instructions=' then
      ParseMaxInstructions(Copy(Arg, Length('--max-instructions=') + 1, MaxInt),
        Result)
    else if Copy(Arg, 1, Length('--profile=')) = '--profile=' then
      ParseProfileMode(Copy(Arg, Length('--profile=') + 1, MaxInt), Result)
    else if Copy(Arg, 1, Length('--profile-output=')) = '--profile-output=' then
      Result.ProfileOutputPath := Copy(Arg, Length('--profile-output=') + 1,
        MaxInt)
    else if Copy(Arg, 1, 2) = '--' then
      raise Exception.Create('Unknown option: ' + Arg)
    else if Result.FileName = STDIN_PATH_MARKER then
      Result.FileName := Arg
    else
      raise Exception.Create('Unexpected argument: ' + Arg);
  end;

  if (not Result.SourceTypeExplicit) and
     IsModuleSourceFileName(Result.FileName) then
    Result.SourceType := stModule;

  if Result.ProfileModePresent then
    Result.Mode := bemBytecode;

  if (Result.ProfileOutputPath <> '') and not Result.ProfileModePresent then
    raise Exception.Create(
      '--profile-output requires --profile=opcodes|functions|all');
end;

function ReadBareSource(const AFileName: string): TStringList;
begin
  if IsStdinPath(AFileName) then
    Result := ReadSourceFromText(Input)
  else
    Result := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
end;

procedure ConfigureEngine(const AEngine: TGocciaEngine;
  const AExecutor: TGocciaExecutor; const AOptions: TBareOptions);
begin
  AEngine.Compatibility := AOptions.Compatibility;
  AEngine.StrictTypes := AOptions.StrictTypes;
  AEngine.SourceType := AOptions.SourceType;
  AEngine.FunctionConstructor.Enabled := AOptions.UnsafeFunctionConstructor;
  if AOptions.Test262Host then
    AEngine.ModuleLoader.SetContentProvider(
      TBareTest262ModuleContentProvider.Create, True);
  if AExecutor is TGocciaBytecodeExecutor then
    TGocciaBytecodeExecutor(AExecutor).GlobalBackedTopLevel :=
      AOptions.SourceType = stScript;
end;

procedure RegisterBareGlobals(const AEngine: TGocciaEngine;
  const APrintHost: TBarePrintHost; const ATest262Host: TBareTest262Host;
  const ATest262EvalHost: TBareTest262EvalHost;
  const AOptions: TBareOptions);
begin
  AEngine.RegisterGlobal(BARE_PRINT_GLOBAL_NAME,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(APrintHost.Print,
      BARE_PRINT_GLOBAL_NAME, -1));
  if AOptions.Test262Host then
    InstallTest262HostGlobals(AEngine, ATest262Host, ATest262EvalHost);
  AEngine.RefreshGlobalThis;
  if AOptions.Test262Host then
    InstallTest262EvalGlobal(AEngine, ATest262EvalHost);
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
  Test262EvalHost: TBareTest262EvalHost;
  Test262Host: TBareTest262Host;
begin
  Result := 0;
  Source := ReadBareSource(AOptions.FileName);
  try
    InitializeProfiler(AOptions);
    if AOptions.SourceName <> '' then
      DisplayName := AOptions.SourceName
    else if IsStdinPath(AOptions.FileName) then
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
      Test262Host := TBareTest262Host.Create(AOptions);
      try
        Executor := CreateExecutorForMode(AOptions.Mode);
        try
          Engine := TGocciaEngine.Create(DisplayName, Source, Executor);
          try
            ConfigureEngine(Engine, Executor, AOptions);
            Test262EvalHost := TBareTest262EvalHost.Create(Engine);
            try

              GC := TGarbageCollector.Instance;
              if Assigned(GC) and (AOptions.MaxMemoryBytes > 0) then
                GC.MaxBytes := AOptions.MaxMemoryBytes;

              RegisterBareGlobals(Engine, PrintHost, Test262Host,
                Test262EvalHost, AOptions);
              try
                ScriptResult := Engine.Execute;
                PrintResult(ScriptResult.Result, AOptions.Print);
              except
                on E: EGocciaBytecodeThrow do
                begin
                  WriteLn(StdErr, FormatThrowDetail(E.ThrownValue,
                    DisplayName, Source, IsColorTerminal));
                  Result := 1;
                end;
                on E: TGocciaThrowValue do
                begin
                  WriteLn(StdErr, FormatThrowDetail(E.Value, DisplayName,
                    Source, IsColorTerminal, E.Suggestion));
                  Result := 1;
                end;
              end;
            finally
              Test262EvalHost.Free;
            end;
          finally
            Engine.Free;
          end;
        finally
          Executor.Free;
        end;
      finally
        Test262Host.Free;
      end;
    finally
      PrintHost.Free;
    end;
  finally
    try
      WriteProfilerReport(AOptions);
    finally
      if Assigned(TGocciaProfiler.Instance) then
        TGocciaProfiler.Shutdown;
      Source.Free;
    end;
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
