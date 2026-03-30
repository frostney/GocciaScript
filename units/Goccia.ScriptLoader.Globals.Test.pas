program Goccia.ScriptLoader.Globals.Test;

{$I Goccia.inc}

uses
  SysUtils,

  GarbageCollector.Generic,
  TestRunner,

  Goccia.ScriptLoader.Globals,
  Goccia.TestSetup,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TScriptLoaderGlobalsTests = class(TTestSuite)
  private
    procedure TestParseGlobalPair;
    procedure TestParseInlineGlobalValueParsesJSON;
    procedure TestParseInlineGlobalValueFallsBackToString;
    procedure TestIsJSONGlobalsFile;
  public
    procedure SetupTests; override;
  end;

procedure TScriptLoaderGlobalsTests.SetupTests;
begin
  Test('Parse global pair', TestParseGlobalPair);
  Test('Parse inline global value parses JSON', TestParseInlineGlobalValueParsesJSON);
  Test('Parse inline global value falls back to string', TestParseInlineGlobalValueFallsBackToString);
  Test('Is JSON globals file', TestIsJSONGlobalsFile);
end;

procedure TScriptLoaderGlobalsTests.TestParseGlobalPair;
var
  Pair: TScriptLoaderGlobalPair;
begin
  Pair := ParseGlobalPair('name={"count":1}');
  Expect<string>(Pair.Key).ToBe('name');
  Expect<string>(Pair.ValueText).ToBe('{"count":1}');
end;

procedure TScriptLoaderGlobalsTests.TestParseInlineGlobalValueParsesJSON;
var
  Value: TGocciaValue;
begin
  Value := ParseInlineGlobalValue('{"count":1,"items":[1,2]}');
  Expect<Boolean>(Value is TGocciaObjectValue).ToBe(True);
  Expect<Boolean>(TGocciaObjectValue(Value).GetProperty('items') is TGocciaArrayValue).ToBe(True);
end;

procedure TScriptLoaderGlobalsTests.TestParseInlineGlobalValueFallsBackToString;
var
  Value: TGocciaValue;
begin
  Value := ParseInlineGlobalValue('plain-text');
  Expect<Boolean>(Value is TGocciaStringLiteralValue).ToBe(True);
  Expect<string>(Value.ToStringLiteral.Value).ToBe('plain-text');
end;

procedure TScriptLoaderGlobalsTests.TestIsJSONGlobalsFile;
begin
  Expect<Boolean>(IsJSONGlobalsFile('context.json')).ToBe(True);
  Expect<Boolean>(IsJSONGlobalsFile('context.js')).ToBe(False);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TScriptLoaderGlobalsTests.Create('ScriptLoader Globals'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
