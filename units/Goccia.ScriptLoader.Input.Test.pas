program Goccia.ScriptLoader.Input.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  GarbageCollector.Generic,
  TestRunner,

  Goccia.ScriptLoader.Input,
  Goccia.TestSetup;

type
  TScriptLoaderInputTests = class(TTestSuite)
  private
    procedure TestReadSourceFromTextPreservesLines;
    procedure TestReadSourceFromTextHandlesEmptyInput;
    procedure TestIsStdinPath;
  public
    procedure SetupTests; override;
  end;

procedure TScriptLoaderInputTests.SetupTests;
begin
  Test('Read source from text preserves lines', TestReadSourceFromTextPreservesLines);
  Test('Read source from text handles empty input', TestReadSourceFromTextHandlesEmptyInput);
  Test('Is stdin path', TestIsStdinPath);
end;

procedure TScriptLoaderInputTests.TestReadSourceFromTextPreservesLines;
var
  TempFileName: string;
  InputFile: Text;
  Source: TStringList;
  RawSource: TFileStream;
  Text: string;
begin
  TempFileName := GetTempFileName;
  Text := 'const x = 2 + 2;' + sLineBreak + 'x;';
  RawSource := TFileStream.Create(TempFileName, fmCreate);
  try
    RawSource.WriteBuffer(Pointer(Text)^, Length(Text));
  finally
    RawSource.Free;
  end;

  AssignFile(InputFile, TempFileName);
  Reset(InputFile);
  try
    Source := ReadSourceFromText(InputFile);
    try
      Expect<Integer>(Source.Count).ToBe(2);
      Expect<string>(Source[0]).ToBe('const x = 2 + 2;');
      Expect<string>(Source[1]).ToBe('x;');
      Expect<string>(Source.Text).ToBe('const x = 2 + 2;' + sLineBreak + 'x;' + sLineBreak);
    finally
      Source.Free;
    end;
  finally
    CloseFile(InputFile);
    DeleteFile(TempFileName);
  end;
end;

procedure TScriptLoaderInputTests.TestReadSourceFromTextHandlesEmptyInput;
var
  TempFileName: string;
  InputFile: Text;
  Source: TStringList;
  EmptySource: TStringList;
begin
  TempFileName := GetTempFileName;
  EmptySource := TStringList.Create;
  try
    EmptySource.SaveToFile(TempFileName);
  finally
    EmptySource.Free;
  end;

  AssignFile(InputFile, TempFileName);
  Reset(InputFile);
  try
    Source := ReadSourceFromText(InputFile);
    try
      Expect<Integer>(Source.Count).ToBe(0);
      Expect<string>(Source.Text).ToBe('');
    finally
      Source.Free;
    end;
  finally
    CloseFile(InputFile);
    DeleteFile(TempFileName);
  end;
end;

procedure TScriptLoaderInputTests.TestIsStdinPath;
begin
  Expect<Boolean>(IsStdinPath('-')).ToBe(True);
  Expect<Boolean>(IsStdinPath(' - ')).ToBe(True);
  Expect<Boolean>(IsStdinPath('script.js')).ToBe(False);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TScriptLoaderInputTests.Create('ScriptLoader Input'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
