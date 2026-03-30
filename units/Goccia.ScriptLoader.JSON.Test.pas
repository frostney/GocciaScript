program Goccia.ScriptLoader.JSON.Test;

{$I Goccia.inc}

uses
  SysUtils,

  GarbageCollector.Generic,
  TestRunner,

  Goccia.Error,
  Goccia.ScriptLoader.JSON,
  Goccia.TestSetup,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Primitives;

type
  TScriptLoaderJSONTests = class(TTestSuite)
  private
    procedure TestBuildSuccessJSONSerializesOutputAndUndefined;
    procedure TestExceptionToScriptLoaderErrorInfoUsesGocciaErrorMetadata;
    procedure TestExceptionToScriptLoaderErrorInfoUsesThrownErrorObject;
  public
    procedure SetupTests; override;
  end;

procedure TScriptLoaderJSONTests.SetupTests;
begin
  Test('Build success JSON serializes output and undefined', TestBuildSuccessJSONSerializesOutputAndUndefined);
  Test('Exception metadata uses Goccia error metadata', TestExceptionToScriptLoaderErrorInfoUsesGocciaErrorMetadata);
  Test('Thrown error object metadata is preserved', TestExceptionToScriptLoaderErrorInfoUsesThrownErrorObject);
end;

procedure TScriptLoaderJSONTests.TestBuildSuccessJSONSerializesOutputAndUndefined;
var
  Timing: TScriptLoaderTiming;
  JSONText: string;
begin
  Timing.LexTimeNanoseconds := 1000000;
  Timing.ParseTimeNanoseconds := 2000000;
  Timing.ExecuteTimeNanoseconds := 3000000;
  Timing.TotalTimeNanoseconds := 6000000;

  JSONText := BuildSuccessJSON(TGocciaUndefinedLiteralValue.UndefinedValue, 'hello' + sLineBreak, Timing);
  Expect<Boolean>(Pos('"ok":true', JSONText) > 0).ToBe(True);
  Expect<Boolean>(Pos('"value":null', JSONText) > 0).ToBe(True);
  Expect<Boolean>(Pos('"output":"hello\n"', JSONText) > 0).ToBe(True);
  Expect<Boolean>(Pos('"lex_ms":1', JSONText) > 0).ToBe(True);
end;

procedure TScriptLoaderJSONTests.TestExceptionToScriptLoaderErrorInfoUsesGocciaErrorMetadata;
var
  ErrorInfo: TScriptLoaderErrorInfo;
  Error: TGocciaSyntaxError;
begin
  Error := TGocciaSyntaxError.Create('Unexpected token', 3, 10, 'script.js', nil);
  try
    ErrorInfo := ExceptionToScriptLoaderErrorInfo(Error);
    Expect<string>(ErrorInfo.ErrorType).ToBe('SyntaxError');
    Expect<string>(ErrorInfo.Message).ToBe('Unexpected token');
    Expect<string>(ErrorInfo.FileName).ToBe('script.js');
    Expect<Integer>(ErrorInfo.Line).ToBe(3);
    Expect<Integer>(ErrorInfo.Column).ToBe(10);
  finally
    Error.Free;
  end;
end;

procedure TScriptLoaderJSONTests.TestExceptionToScriptLoaderErrorInfoUsesThrownErrorObject;
var
  ErrorInfo: TScriptLoaderErrorInfo;
  DidThrow: Boolean;
  ErrorTypeText: string;
  MessageText: string;
  ErrorLine: Integer;
  ErrorColumn: Integer;
begin
  DidThrow := False;
  ErrorTypeText := '';
  MessageText := '';
  ErrorLine := 0;
  ErrorColumn := 0;
  try
    ThrowTypeError('Boom');
  except
    on E: TGocciaThrowValue do
    begin
      DidThrow := True;
      ErrorInfo := ExceptionToScriptLoaderErrorInfo(E);
      ErrorTypeText := ErrorInfo.ErrorType;
      MessageText := ErrorInfo.Message;
      ErrorLine := ErrorInfo.Line;
      ErrorColumn := ErrorInfo.Column;
    end;
  end;
  Expect<Boolean>(DidThrow).ToBe(True);
  Expect<string>(ErrorTypeText).ToBe('TypeError');
  Expect<string>(MessageText).ToBe('Boom');
  Expect<Integer>(ErrorLine).ToBe(-1);
  Expect<Integer>(ErrorColumn).ToBe(-1);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TScriptLoaderJSONTests.Create('ScriptLoader JSON'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
