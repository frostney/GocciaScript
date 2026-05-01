program Goccia.ScriptLoader.Globals.Test;

{$I Goccia.inc}

uses
  Classes,
  Math,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.ModuleResolver,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.ScriptLoader.Globals,
  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TScriptLoaderGlobalsTests = class(TTestSuite)
  private
    FCustomRuntimeLoaderCalled: Boolean;
    function CreateEmptySource: TStringList;
    function LoadCustomRuntimeModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean;
    procedure TestDetectsStructuredGlobalsFilesByExtension;
    procedure TestRuntimeConstructorAcceptsExistingEngine;
    procedure TestRuntimeConstructorRejectsDifferentGlobals;
    procedure TestRuntimePreservesResolverExtensions;
    procedure TestRuntimeModuleLoaderFallsBackToPreviousLoader;
    procedure TestEngineInjectGlobalsFromJSON5;
    procedure TestReadFileTextPreservesUTF8ForTOMLGlobals;
    procedure TestEngineInjectGlobalsFromTOML;
    procedure TestBytecodeBackendInjectGlobalsFromJSON5;
    procedure TestBytecodeBackendInjectGlobalsFromTOML;
  public
    procedure SetupTests; override;
  end;

procedure TScriptLoaderGlobalsTests.SetupTests;
begin
  Test('Detects structured globals files by extension',
    TestDetectsStructuredGlobalsFilesByExtension);
  Test('ReadFileText preserves UTF-8 for TOML globals',
    TestReadFileTextPreservesUTF8ForTOMLGlobals);
  Test('Runtime constructor accepts existing engine',
    TestRuntimeConstructorAcceptsExistingEngine);
  Test('Runtime constructor rejects different globals',
    TestRuntimeConstructorRejectsDifferentGlobals);
  Test('Runtime preserves resolver extensions',
    TestRuntimePreservesResolverExtensions);
  Test('Runtime module loader falls back to previous loader',
    TestRuntimeModuleLoaderFallsBackToPreviousLoader);
  Test('Engine injects globals from JSON5',
    TestEngineInjectGlobalsFromJSON5);
  Test('Engine injects globals from TOML',
    TestEngineInjectGlobalsFromTOML);
  Test('Bytecode backend injects globals from JSON5',
    TestBytecodeBackendInjectGlobalsFromJSON5);
  Test('Bytecode backend injects globals from TOML',
    TestBytecodeBackendInjectGlobalsFromTOML);
end;

function TScriptLoaderGlobalsTests.CreateEmptySource: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := '';
end;

function TScriptLoaderGlobalsTests.LoadCustomRuntimeModule(
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

procedure TScriptLoaderGlobalsTests.TestDetectsStructuredGlobalsFilesByExtension;
begin
  Expect<Boolean>(IsStructuredGlobalsFile('config.json5')).ToBe(True);
  Expect<Boolean>(IsJSON5GlobalsFile('config.json5')).ToBe(True);
  Expect<Boolean>(IsStructuredGlobalsFile('config.jsonc')).ToBe(True);
  Expect<Boolean>(IsJSON5GlobalsFile('config.jsonc')).ToBe(True);
  Expect<Boolean>(IsStructuredGlobalsFile('config.toml')).ToBe(True);
  Expect<Boolean>(IsTOMLGlobalsFile('config.toml')).ToBe(True);
  Expect<Boolean>(IsYAMLGlobalsFile('config.toml')).ToBe(False);
end;

procedure TScriptLoaderGlobalsTests.TestRuntimeConstructorAcceptsExistingEngine;
var
  Engine: TGocciaEngine;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<runtime-test>', Source, []);
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
end;

procedure TScriptLoaderGlobalsTests.TestRuntimeConstructorRejectsDifferentGlobals;
var
  Engine: TGocciaEngine;
  FirstRuntime: TGocciaRuntime;
  HasExpectedMessage: Boolean;
  RaisedExpected: Boolean;
  SecondRuntime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<runtime-test>', Source, []);
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
end;

procedure TScriptLoaderGlobalsTests.TestRuntimePreservesResolverExtensions;
var
  Engine: TGocciaEngine;
  Extensions: TModuleResolverExtensionArray;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<runtime-test>', Source, []);
  Runtime := nil;
  try
    Engine.Resolver.SetExtensions(['.custom', '.js']);
    Runtime := TGocciaRuntime.Create(Engine, [rgJSON5]);
    Extensions := Engine.Resolver.GetExtensions;

    Expect<string>(Extensions[0]).ToBe('.custom');
    Expect<string>(Extensions[1]).ToBe('.js');
    Expect<string>(Extensions[High(Extensions) - 1]).ToBe('.json5');
    Expect<string>(Extensions[High(Extensions)]).ToBe('.jsonc');
  finally
    Runtime.Free;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TScriptLoaderGlobalsTests.TestRuntimeModuleLoaderFallsBackToPreviousLoader;
var
  Engine: TGocciaEngine;
  LoadedModule: TGocciaModule;
  Runtime: TGocciaRuntime;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<runtime-test>', Source, []);
  Runtime := nil;
  LoadedModule := nil;
  FCustomRuntimeLoaderCalled := False;
  try
    Engine.ModuleLoader.RuntimeModuleLoader := LoadCustomRuntimeModule;
    Runtime := TGocciaRuntime.Create(Engine, [rgJSON5]);

    Expect<Boolean>(Engine.ModuleLoader.RuntimeModuleLoader(
      'virtual.custom', LoadedModule)).ToBe(True);
    Expect<Boolean>(FCustomRuntimeLoaderCalled).ToBe(True);
    Expect<string>(LoadedModule.Path).ToBe('virtual.custom');
  finally
    LoadedModule.Free;
    Runtime.Free;
    Engine.Free;
    Source.Free;
  end;
end;

procedure TScriptLoaderGlobalsTests.TestReadFileTextPreservesUTF8ForTOMLGlobals;
var
  Engine: TGocciaEngine;
  NameValue: UTF8String;
  QuotedKey: UTF8String;
  Source: TStringList;
  Stream: TFileStream;
  TempFileName: string;
  TextBytes: UTF8String;
begin
  NameValue := 'Jos' + #$C3#$A9;
  QuotedKey := 'd' + #$C3#$A9 + 'j' + #$C3#$A0;
  TextBytes :=
    'name = "' + NameValue + '"' + #10 +
    '"' + QuotedKey + '" = "vu"' + #10;
  TempFileName := GetTempFileName(GetTempDir(False), 'goc');

  Stream := TFileStream.Create(TempFileName, fmCreate);
  try
    if Length(TextBytes) > 0 then
      Stream.WriteBuffer(Pointer(TextBytes)^, Length(TextBytes));
  finally
    Stream.Free;
  end;

  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<globals-test>', Source,
    []);
  try
    AttachRuntimeExtension(Engine);
    Engine.InjectGlobalsFromTOML(ReadFileText(TempFileName));

    Expect<string>(Engine.Interpreter.GlobalScope.GetValue('name')
      .ToStringLiteral.Value).ToBe(NameValue);
    Expect<string>(Engine.Interpreter.GlobalScope.GetValue(QuotedKey)
      .ToStringLiteral.Value).ToBe('vu');
  finally
    Engine.Free;
    Source.Free;
    DeleteFile(TempFileName);
  end;
end;

procedure TScriptLoaderGlobalsTests.TestEngineInjectGlobalsFromJSON5;
var
  Engine: TGocciaEngine;
  Obj: TGocciaObjectValue;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<globals-test>', Source,
    []);
  try
    AttachRuntimeExtension(Engine);
    Engine.InjectGlobalsFromJSON5(
      '{' + LineEnding +
      '  // comment' + LineEnding +
      '  unquoted: ''goccia'',' + LineEnding +
      '  maxRetries: 3,' + LineEnding +
      '  nested: { enabled: true, },' + LineEnding +
      '}');

    Expect<string>(Engine.Interpreter.GlobalScope.GetValue('unquoted')
      .ToStringLiteral.Value).ToBe('goccia');
    Expect<Double>(Engine.Interpreter.GlobalScope.GetValue('maxRetries')
      .ToNumberLiteral.Value).ToBe(3);

    Obj := TGocciaObjectValue(Engine.Interpreter.GlobalScope.GetValue('nested'));
    Expect<Boolean>(Obj.GetProperty('enabled').ToBooleanLiteral.Value)
      .ToBe(True);
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TScriptLoaderGlobalsTests.TestEngineInjectGlobalsFromTOML;
var
  Arr: TGocciaArrayValue;
  Engine: TGocciaEngine;
  Obj: TGocciaObjectValue;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<globals-test>', Source,
    []);
  try
    AttachRuntimeExtension(Engine);
    Engine.InjectGlobalsFromTOML(
      'name = "goccia"' + LineEnding +
      'count = 3' + LineEnding +
      'tags = ["alpha", "beta"]' + LineEnding +
      '[database]' + LineEnding +
      'host = "localhost"' + LineEnding);

    Expect<string>(Engine.Interpreter.GlobalScope.GetValue('name')
      .ToStringLiteral.Value).ToBe('goccia');
    Expect<Double>(Engine.Interpreter.GlobalScope.GetValue('count')
      .ToNumberLiteral.Value).ToBe(3);

    Arr := TGocciaArrayValue(Engine.Interpreter.GlobalScope.GetValue('tags'));
    Expect<Integer>(Arr.Elements.Count).ToBe(2);
    Expect<string>(Arr.Elements[1].ToStringLiteral.Value).ToBe('beta');

    Obj := TGocciaObjectValue(Engine.Interpreter.GlobalScope.GetValue('database'));
    Expect<string>(Obj.GetProperty('host').ToStringLiteral.Value)
      .ToBe('localhost');
  finally
    Engine.Free;
    Source.Free;
  end;
end;

procedure TScriptLoaderGlobalsTests.TestBytecodeBackendInjectGlobalsFromTOML;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  Obj: TGocciaObjectValue;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := TGocciaEngine.Create('<globals-test>', Source, [], Executor);
  try
    AttachRuntimeExtension(Engine);
    Engine.InjectGlobalsFromTOML(
      'name = "goccia"' + LineEnding +
      'release = 2026-04-04T12:30:45Z' + LineEnding +
      '[database]' + LineEnding +
      'port = 5432' + LineEnding);

    Expect<string>(Engine.Interpreter.GlobalScope.GetValue('name')
      .ToStringLiteral.Value).ToBe('goccia');
    Expect<string>(Engine.Interpreter.GlobalScope.GetValue('release')
      .ToStringLiteral.Value).ToBe('2026-04-04T12:30:45Z');

    Obj := TGocciaObjectValue(Engine.Interpreter.GlobalScope.GetValue('database'));
    Expect<Double>(Obj.GetProperty('port').ToNumberLiteral.Value).ToBe(5432);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

procedure TScriptLoaderGlobalsTests.TestBytecodeBackendInjectGlobalsFromJSON5;
var
  Engine: TGocciaEngine;
  Executor: TGocciaBytecodeExecutor;
  MaxValue, MinValue: TGocciaNumberLiteralValue;
  Obj: TGocciaObjectValue;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Executor := TGocciaBytecodeExecutor.Create;
  Engine := TGocciaEngine.Create('<globals-test>', Source, [], Executor);
  try
    AttachRuntimeExtension(Engine);
    Engine.InjectGlobalsFromJSON5(
      '{' + LineEnding +
      '  answer: +42,' + LineEnding +
      '  limits: { max: Infinity, min: -Infinity, },' + LineEnding +
      '}');

    Expect<Double>(Engine.Interpreter.GlobalScope.GetValue('answer')
      .ToNumberLiteral.Value).ToBe(42);

    Obj := TGocciaObjectValue(Engine.Interpreter.GlobalScope.GetValue('limits'));
    MaxValue := Obj.GetProperty('max').ToNumberLiteral;
    MinValue := Obj.GetProperty('min').ToNumberLiteral;
    Expect<Boolean>(MaxValue.IsInfinity).ToBe(True);
    Expect<Boolean>(MinValue.IsNegativeInfinity).ToBe(True);
  finally
    Engine.Free;
    Executor.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TScriptLoaderGlobalsTests.Create(
    'ScriptLoader Globals'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
