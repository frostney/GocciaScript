program Goccia.ScriptLoader.Globals.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestRunner,

  Goccia.Engine,
  Goccia.Engine.Backend,
  Goccia.ScriptLoader.Globals,
  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TScriptLoaderGlobalsTests = class(TTestSuite)
  private
    function CreateEmptySource: TStringList;
    procedure TestDetectsTOMLGlobalsFileByExtension;
    procedure TestReadFileTextPreservesUTF8ForTOMLGlobals;
    procedure TestEngineInjectGlobalsFromTOML;
    procedure TestBytecodeBackendInjectGlobalsFromTOML;
  public
    procedure SetupTests; override;
  end;

procedure TScriptLoaderGlobalsTests.SetupTests;
begin
  Test('Detects TOML globals file by extension',
    TestDetectsTOMLGlobalsFileByExtension);
  Test('ReadFileText preserves UTF-8 for TOML globals',
    TestReadFileTextPreservesUTF8ForTOMLGlobals);
  Test('Engine injects globals from TOML',
    TestEngineInjectGlobalsFromTOML);
  Test('Bytecode backend injects globals from TOML',
    TestBytecodeBackendInjectGlobalsFromTOML);
end;

function TScriptLoaderGlobalsTests.CreateEmptySource: TStringList;
begin
  Result := TStringList.Create;
  Result.Text := '';
end;

procedure TScriptLoaderGlobalsTests.TestDetectsTOMLGlobalsFileByExtension;
begin
  Expect<Boolean>(IsStructuredGlobalsFile('config.toml')).ToBe(True);
  Expect<Boolean>(IsTOMLGlobalsFile('config.toml')).ToBe(True);
  Expect<Boolean>(IsYAMLGlobalsFile('config.toml')).ToBe(False);
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
    TGocciaEngine.DefaultGlobals);
  try
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

procedure TScriptLoaderGlobalsTests.TestEngineInjectGlobalsFromTOML;
var
  Arr: TGocciaArrayValue;
  Engine: TGocciaEngine;
  Obj: TGocciaObjectValue;
  Source: TStringList;
begin
  Source := CreateEmptySource;
  Engine := TGocciaEngine.Create('<globals-test>', Source,
    TGocciaEngine.DefaultGlobals);
  try
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
  Backend: TGocciaBytecodeBackend;
  Obj: TGocciaObjectValue;
begin
  Backend := TGocciaBytecodeBackend.Create('<globals-test>');
  try
    Backend.RegisterBuiltIns(TGocciaEngine.DefaultGlobals);
    Backend.InjectGlobalsFromTOML(
      'name = "goccia"' + LineEnding +
      'release = 2026-04-04T12:30:45Z' + LineEnding +
      '[database]' + LineEnding +
      'port = 5432' + LineEnding);

    Expect<string>(Backend.Interpreter.GlobalScope.GetValue('name')
      .ToStringLiteral.Value).ToBe('goccia');
    Expect<string>(Backend.Interpreter.GlobalScope.GetValue('release')
      .ToStringLiteral.Value).ToBe('2026-04-04T12:30:45Z');

    Obj := TGocciaObjectValue(Backend.Interpreter.GlobalScope.GetValue('database'));
    Expect<Double>(Obj.GetProperty('port').ToNumberLiteral.Value).ToBe(5432);
  finally
    Backend.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TScriptLoaderGlobalsTests.Create(
    'ScriptLoader Globals'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
