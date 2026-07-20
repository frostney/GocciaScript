program Goccia.Runtime.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  FileUtils,
  TestingPascalLibrary,

  Goccia.Constants.PropertyNames,
  Goccia.Engine,
  Goccia.Error.Messages,
  Goccia.Executor.Interpreter,
  Goccia.ModuleResolver,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Realm,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.Console,
  Goccia.RuntimeExtensions.JSON5,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  ROLLBACK_PROBE_NAME = 'resolverRollbackProbe';

type
  TCustomRuntimeModuleResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath,
      AImportingFilePath: string): string; override;
  end;

  TRollbackModuleRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FPreviousProbeValue: TGocciaValue;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
    procedure AddModuleExtensions(const AExtensions: TStrings); override;
  end;

  TRuntimeTests = class(TTestSuite)
  private
    FCustomRuntimeLoaderCalled: Boolean;
    FProvidedGlobalModule: TGocciaModule;
    FReplacementGlobalModule: TGocciaModule;
    function CreateEmptySource: TStringList;
    function CreateProvidedGlobalModule: TGocciaModule;
    function CreateReplacementGlobalModule: TGocciaModule;
    function LoadCustomRuntimeModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean;
    procedure TestEngineRejectsNilExtension;
    procedure TestGlobalModuleProviderReplacementClearsLoadedModule;
    procedure TestGlobalModuleProviderUnregisterClearsLoadedModule;
    procedure TestRuntimeConstructorAcceptsExistingEngine;
    procedure TestRuntimeInstallIsIdempotent;
    procedure TestRuntimeInstallRollsBackResolverExtensions;
    procedure TestRuntimePreservesResolverExtensions;
    procedure TestRuntimeModuleLoaderFallsBackToPreviousLoader;
    procedure TestRuntimeRunScriptFromFileLoadsFile;
  public
    procedure SetupTests; override;
  end;

function TCustomRuntimeModuleResolver.Resolve(const AModulePath,
  AImportingFilePath: string): string;
begin
  if AModulePath = 'virtual.custom' then
    Exit('virtual.custom');

  Result := inherited Resolve(AModulePath, AImportingFilePath);
end;

procedure TRollbackModuleRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited;
  FPreviousProbeValue := ARuntime.Engine.Interpreter.GlobalScope.GetValue(
    ROLLBACK_PROBE_NAME);
  ARuntime.Engine.RegisterGlobal(ROLLBACK_PROBE_NAME,
    TGocciaUndefinedLiteralValue.UndefinedValue);
  TGocciaObjectValue(ARuntime.Engine.Realm.GlobalObject).PreventExtensions;
end;

procedure TRollbackModuleRuntimeExtension.Detach;
begin
  if Assigned(Runtime) then
    Runtime.Engine.RegisterGlobal(ROLLBACK_PROBE_NAME, FPreviousProbeValue);
  FPreviousProbeValue := nil;
  inherited;
end;

procedure TRollbackModuleRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
  AExtensions.Add('.rollback-test');
end;

procedure TRuntimeTests.SetupTests;
begin
  Test('Engine rejects nil extension',
    TestEngineRejectsNilExtension);
  Test('Global module provider unregister clears loaded module',
    TestGlobalModuleProviderUnregisterClearsLoadedModule);
  Test('Global module provider replacement clears loaded module',
    TestGlobalModuleProviderReplacementClearsLoadedModule);
  Test('Runtime constructor accepts existing engine',
    TestRuntimeConstructorAcceptsExistingEngine);
  Test('Runtime extension install is idempotent',
    TestRuntimeInstallIsIdempotent);
  Test('Runtime extension install rolls back resolver extensions',
    TestRuntimeInstallRollsBackResolverExtensions);
  Test('Runtime preserves resolver extensions',
    TestRuntimePreservesResolverExtensions);
  Test('Runtime module loader falls back to previous loader',
    TestRuntimeModuleLoaderFallsBackToPreviousLoader);
  Test('Runtime RunScriptFromFile loads file',
    TestRuntimeRunScriptFromFileLoadsFile);
end;

function TRuntimeTests.CreateEmptySource: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := '';
end;

function TRuntimeTests.CreateProvidedGlobalModule: TGocciaModule;
begin
  if not Assigned(FProvidedGlobalModule) then
    FProvidedGlobalModule := TGocciaModule.Create('virtual:provider');
  Result := FProvidedGlobalModule;
end;

function TRuntimeTests.CreateReplacementGlobalModule: TGocciaModule;
begin
  if not Assigned(FReplacementGlobalModule) then
    FReplacementGlobalModule := TGocciaModule.Create('virtual:provider');
  Result := FReplacementGlobalModule;
end;

function TRuntimeTests.LoadCustomRuntimeModule(
  const AResolvedPath: string; out AModule: TGocciaModule): Boolean;
begin
  Result := AResolvedPath = 'virtual.custom';
  if Result then
  begin
    FCustomRuntimeLoaderCalled := True;
    AModule := TGocciaModule.Create(AResolvedPath);
  end
  else
    AModule := nil;
end;

procedure TRuntimeTests.TestEngineRejectsNilExtension;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  HasExpectedMessage: Boolean;
  RaisedExpected: Boolean;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<extension-test>', Source, Executor);
    try
      RaisedExpected := False;
      HasExpectedMessage := False;
      try
        Engine.AddExtension(nil);
        Fail('Expected nil extension registration to raise an exception.');
      except
        on E: Exception do
        begin
          RaisedExpected := True;
          HasExpectedMessage := Pos('extension cannot be nil', E.Message) > 0;
          if not HasExpectedMessage then
            Fail('Expected nil extension error message.');
        end;
      end;

      Expect<Boolean>(RaisedExpected).ToBe(True);
    finally
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestGlobalModuleProviderUnregisterClearsLoadedModule;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  LoadedModule: TGocciaModule;
  RaisedExpected: Boolean;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := nil;
  FProvidedGlobalModule := nil;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    Engine.RegisterGlobalModuleProvider('virtual:provider',
      CreateProvidedGlobalModule);

    LoadedModule := Engine.ModuleLoader.LoadModule('virtual:provider',
      '<runtime-test>');
    Expect<Boolean>(LoadedModule = FProvidedGlobalModule).ToBe(True);
    Expect<Boolean>(
      Engine.ModuleLoader.GlobalModules.ContainsKey('virtual:provider')).ToBe(True);

    Engine.UnregisterGlobalModuleProvider('virtual:provider');
    Expect<Boolean>(
      Engine.ModuleLoader.GlobalModules.ContainsKey('virtual:provider')).ToBe(False);

    RaisedExpected := False;
    try
      Engine.ModuleLoader.LoadModule('virtual:provider', '<runtime-test>');
      Fail('Expected unregistered global module provider to stop resolving.');
    except
      on Exception do
        RaisedExpected := True;
    end;
    Expect<Boolean>(RaisedExpected).ToBe(True);
  finally
    if Assigned(Engine) then
      Engine.UnregisterGlobalModuleProvider('virtual:provider');
    FProvidedGlobalModule.Free;
    FProvidedGlobalModule := nil;
    Engine.Free;
    Source.Free;
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestGlobalModuleProviderReplacementClearsLoadedModule;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  LoadedModule: TGocciaModule;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  Engine := nil;
  FProvidedGlobalModule := nil;
  FReplacementGlobalModule := nil;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    Engine.RegisterGlobalModuleProvider('virtual:provider',
      CreateProvidedGlobalModule);

    LoadedModule := Engine.ModuleLoader.LoadModule('virtual:provider',
      '<runtime-test>');
    Expect<Boolean>(LoadedModule = FProvidedGlobalModule).ToBe(True);
    Expect<Boolean>(
      Engine.ModuleLoader.GlobalModules.ContainsKey('virtual:provider')).ToBe(True);

    Engine.RegisterGlobalModuleProvider('virtual:provider',
      CreateReplacementGlobalModule);
    Expect<Boolean>(
      Engine.ModuleLoader.GlobalModules.ContainsKey('virtual:provider')).ToBe(False);

    LoadedModule := Engine.ModuleLoader.LoadModule('virtual:provider',
      '<runtime-test>');
    Expect<Boolean>(LoadedModule = FReplacementGlobalModule).ToBe(True);
    Expect<Boolean>(LoadedModule = FProvidedGlobalModule).ToBe(False);
  finally
    if Assigned(Engine) then
      Engine.UnregisterGlobalModuleProvider('virtual:provider');
    FProvidedGlobalModule.Free;
    FProvidedGlobalModule := nil;
    FReplacementGlobalModule.Free;
    FReplacementGlobalModule := nil;
    Engine.Free;
    Source.Free;
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimeConstructorAcceptsExistingEngine;
var
  ConsoleExtension: TGocciaConsoleRuntimeExtension;
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    Runtime := nil;
    try
      Runtime := TGocciaRuntime.Create(Engine);
      Runtime.Install(TGocciaConsoleRuntimeExtension.Create);
      ConsoleExtension := TGocciaConsoleRuntimeExtension(
        Runtime.FindRuntimeExtension(TGocciaConsoleRuntimeExtension));

      Expect<Boolean>(Runtime.Engine = Engine).ToBe(True);
      Expect<Boolean>(Assigned(ConsoleExtension)).ToBe(True);
      Expect<Boolean>(Assigned(ConsoleExtension.BuiltinConsole)).ToBe(True);
      Expect<Boolean>(Assigned(
        Runtime.FindRuntimeExtension(TGocciaJSON5RuntimeExtension))).ToBe(False);
    finally
      Runtime.Free;
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimeInstallIsIdempotent;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  FirstExtension: TGocciaRuntimeExtension;
  Runtime: TGocciaRuntime;
  SecondExtension: TGocciaRuntimeExtension;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    Runtime := nil;
    try
      Runtime := TGocciaRuntime.Create(Engine);
      FirstExtension := Runtime.Install(TGocciaConsoleRuntimeExtension.Create);
      SecondExtension := Runtime.Install(TGocciaConsoleRuntimeExtension.Create);

      Expect<Boolean>(FirstExtension = SecondExtension).ToBe(True);
    finally
      Runtime.Free;
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimeInstallRollsBackResolverExtensions;
var
  BaselineExtensions: TModuleResolverExtensionArray;
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Extensions: TModuleResolverExtensionArray;
  I: Integer;
  ProbeValue: TGocciaValue;
  RaisedMessage: string;
  RaisedExpectedException: Boolean;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    Runtime := nil;
    try
      Engine.RegisterGlobal(ROLLBACK_PROBE_NAME,
        TGocciaBooleanLiteralValue.TrueValue);
      Engine.Resolver.SetExtensions(['.custom']);
      Runtime := TGocciaRuntime.Create(Engine);
      BaselineExtensions := Engine.Resolver.GetExtensions;

      RaisedExpectedException := False;
      RaisedMessage := '';
      try
        Runtime.Install(TRollbackModuleRuntimeExtension.Create);
      except
        on E: TGocciaThrowValue do
        begin
          RaisedExpectedException := True;
          if E.Value is TGocciaObjectValue then
            RaisedMessage := TGocciaObjectValue(E.Value)
              .GetProperty(PROP_MESSAGE).ToStringLiteral.Value;
        end;
      end;
      Expect<Boolean>(RaisedExpectedException).ToBe(True);
      Expect<string>(RaisedMessage).ToBe(Format(
        SErrorCannotAddPropertyNotExtensible, [ROLLBACK_PROBE_NAME]));

      Extensions := Engine.Resolver.GetExtensions;
      Expect<Integer>(Length(Extensions)).ToBe(Length(BaselineExtensions));
      for I := 0 to High(Extensions) do
        Expect<string>(Extensions[I]).ToBe(BaselineExtensions[I]);
      Expect<Boolean>(not Assigned(Runtime.FindRuntimeExtension(
        TRollbackModuleRuntimeExtension))).ToBe(True);
      ProbeValue := Engine.Interpreter.GlobalScope.GetValue(
        ROLLBACK_PROBE_NAME);
      Expect<Boolean>(ProbeValue =
        TGocciaBooleanLiteralValue.TrueValue).ToBe(True);
    finally
      Runtime.Free;
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimePreservesResolverExtensions;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  Extensions: TModuleResolverExtensionArray;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    Runtime := nil;
    try
      Engine.Resolver.SetExtensions(['.custom', '.js']);
      Runtime := TGocciaRuntime.Create(Engine);
      Runtime.Install(TGocciaJSON5RuntimeExtension.Create);
      Extensions := Engine.Resolver.GetExtensions;

      Expect<Boolean>(Length(Extensions) >= 4).ToBe(True);
      Expect<string>(Extensions[0]).ToBe('.custom');
      Expect<string>(Extensions[1]).ToBe('.js');
      Expect<string>(Extensions[High(Extensions) - 1]).ToBe('.json5');
      Expect<string>(Extensions[High(Extensions)]).ToBe('.jsonc');
    finally
      Runtime.Free;
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimeModuleLoaderFallsBackToPreviousLoader;
var
  CachedModule: TGocciaModule;
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  LoadedModule: TGocciaModule;
  ModuleLoader: TGocciaModuleLoader;
  Resolver: TCustomRuntimeModuleResolver;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Resolver := nil;
  ModuleLoader := nil;
  Engine := nil;
  Runtime := nil;
  LoadedModule := nil;
  FCustomRuntimeLoaderCalled := False;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Resolver := TCustomRuntimeModuleResolver.Create;
    ModuleLoader := TGocciaModuleLoader.Create('<runtime-test>', Resolver);
    Engine := TGocciaEngine.Create('<runtime-test>', Source, ModuleLoader, Executor);
    Engine.ModuleLoader.RuntimeModuleLoader := LoadCustomRuntimeModule;
    Runtime := TGocciaRuntime.Create(Engine);
    Runtime.Install(TGocciaJSON5RuntimeExtension.Create);

    LoadedModule := Engine.ModuleLoader.LoadModule(
      'virtual.custom', '<runtime-test>');
    CachedModule := Engine.ModuleLoader.LoadModule(
      'virtual.custom', '<runtime-test>');

    Expect<Boolean>(FCustomRuntimeLoaderCalled).ToBe(True);
    Expect<string>(LoadedModule.Path).ToBe('virtual.custom');
    Expect<Boolean>(CachedModule = LoadedModule).ToBe(True);
  finally
    Runtime.Free;
    Engine.Free;
    ModuleLoader.Free;
    Resolver.Free;
    Source.Free;
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimeRunScriptFromFileLoadsFile;
var
  ScriptResult: TGocciaScriptResult;
  TempFileName: string;
begin
  TempFileName := GetTempFileName(GetTempDir(False), 'goc');
  WriteUTF8FileText(TempFileName, '21 * 2;');

  try
    ScriptResult := TGocciaRuntime.RunScriptFromFile(TempFileName);
    Expect<Double>(ScriptResult.Result.ToNumberLiteral.Value).ToBe(42);
  finally
    DeleteFile(TempFileName);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TRuntimeTests.Create('Runtime'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
