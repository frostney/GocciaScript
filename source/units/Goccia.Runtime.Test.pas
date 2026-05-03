program Goccia.Runtime.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.ModuleResolver,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.Modules.Resolver,
  Goccia.Runtime,
  Goccia.TestSetup,
  Goccia.Values.Primitives;

type
  TCustomRuntimeModuleResolver = class(TGocciaModuleResolver)
  public
    function Resolve(const AModulePath,
      AImportingFilePath: string): string; override;
  end;

  TRuntimeTests = class(TTestSuite)
  private
    FCustomRuntimeLoaderCalled: Boolean;
    function CreateEmptySource: TStringList;
    function LoadCustomRuntimeModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean;
    procedure TestEngineRejectsNilExtension;
    procedure TestRuntimeConstructorAcceptsExistingEngine;
    procedure TestRuntimeConstructorRejectsDifferentGlobals;
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

procedure TRuntimeTests.SetupTests;
begin
  Test('Engine rejects nil extension',
    TestEngineRejectsNilExtension);
  Test('Runtime constructor accepts existing engine',
    TestRuntimeConstructorAcceptsExistingEngine);
  Test('Runtime constructor rejects different globals',
    TestRuntimeConstructorRejectsDifferentGlobals);
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

procedure TRuntimeTests.TestRuntimeConstructorAcceptsExistingEngine;
var
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
      Runtime := TGocciaRuntime.Create(Engine, [rgConsole]);

      Expect<Boolean>(Runtime.Engine = Engine).ToBe(True);
      Expect<Boolean>(Assigned(Runtime.BuiltinConsole)).ToBe(True);
      Expect<Boolean>(rgConsole in Runtime.Globals).ToBe(True);
      Expect<Boolean>(rgSemver in Runtime.Globals).ToBe(False);
    finally
      Runtime.Free;
      Engine.Free;
      Source.Free;
    end;
  finally
    Executor.Free;
  end;
end;

procedure TRuntimeTests.TestRuntimeConstructorRejectsDifferentGlobals;
var
  Engine: TGocciaEngine;
  Executor: TGocciaInterpreterExecutor;
  FirstRuntime: TGocciaRuntime;
  HasExpectedMessage: Boolean;
  RaisedExpected: Boolean;
  SecondRuntime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaInterpreterExecutor.Create;
  try
    Engine := TGocciaEngine.Create('<runtime-test>', Source, Executor);
    FirstRuntime := nil;
    SecondRuntime := nil;
    try
      FirstRuntime := TGocciaRuntime.Create(Engine, [rgConsole]);
      RaisedExpected := False;
      HasExpectedMessage := False;

      try
        SecondRuntime := TGocciaRuntime.Create(Engine, [rgConsole, rgSemver]);
        Fail('Expected runtime global mismatch to raise an exception.');
      except
        on E: Exception do
        begin
          RaisedExpected := True;
          HasExpectedMessage := Pos('different global configuration',
            E.Message) > 0;
          if not HasExpectedMessage then
            Fail('Expected runtime global mismatch error message.');
        end;
      end;

      Expect<Boolean>(RaisedExpected).ToBe(True);
    finally
      SecondRuntime.Free;
      FirstRuntime.Free;
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
      Runtime := TGocciaRuntime.Create(Engine, [rgJSON5]);
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
    Runtime := TGocciaRuntime.Create(Engine, [rgJSON5]);

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
  SourceBytes: UTF8String;
  Stream: TFileStream;
  TempFileName: string;
begin
  SourceBytes := '21 * 2;';
  TempFileName := GetTempFileName(GetTempDir(False), 'goc');

  Stream := TFileStream.Create(TempFileName, fmCreate);
  try
    Stream.WriteBuffer(Pointer(SourceBytes)^, Length(SourceBytes));
  finally
    Stream.Free;
  end;

  try
    ScriptResult := TGocciaRuntime.RunScriptFromFile(TempFileName,
      [rgConsole]);
    Expect<Double>(ScriptResult.Result.ToNumberLiteral.Value).ToBe(42);
  finally
    DeleteFile(TempFileName);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TRuntimeTests.Create('Runtime'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
