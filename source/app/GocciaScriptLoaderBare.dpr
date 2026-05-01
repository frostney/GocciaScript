program GocciaScriptLoaderBare;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TextSemantics,

  Goccia.Engine,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.ScriptLoader.Input,
  Goccia.Terminal.Colors,
  Goccia.TextFiles,
  Goccia.Values.Error,
  Goccia.Values.Primitives;

type
  TBareOptions = record
    ASI: Boolean;
    CompatVar: Boolean;
    CompatFunction: Boolean;
    StrictTypes: Boolean;
    UnsafeFunctionConstructor: Boolean;
    SourceType: TGocciaSourceType;
    FileName: string;
  end;

procedure PrintUsage;
begin
  WriteLn('Usage: GocciaScriptLoaderBare [file|-] [options]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  --asi                         Enable automatic semicolon insertion');
  WriteLn('  --compat-var                  Enable var declarations');
  WriteLn('  --compat-function             Enable function declarations/expressions');
  WriteLn('  --strict-types                Enforce type annotations at runtime');
  WriteLn('  --source-type=script|module   Load entry as a script or module');
  WriteLn('  --unsafe-function-constructor Enable dynamic Function constructor');
  WriteLn('  --help                        Show this help');
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

function ParseOptions: TBareOptions;
var
  I: Integer;
  Arg: string;
begin
  Result.ASI := False;
  Result.CompatVar := False;
  Result.CompatFunction := False;
  Result.StrictTypes := False;
  Result.UnsafeFunctionConstructor := False;
  Result.SourceType := stScript;
  Result.FileName := STDIN_PATH_MARKER;

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
    else if Arg = '--strict-types' then
      Result.StrictTypes := True
    else if Arg = '--unsafe-function-constructor' then
      Result.UnsafeFunctionConstructor := True
    else if Copy(Arg, 1, Length('--source-type=')) = '--source-type=' then
      ParseSourceType(Copy(Arg, Length('--source-type=') + 1, MaxInt), Result)
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
  AEngine.StrictTypes := AOptions.StrictTypes;
  AEngine.SourceType := AOptions.SourceType;
  AEngine.FunctionConstructor.Enabled := AOptions.UnsafeFunctionConstructor;
end;

procedure PrintResult(const AResult: TGocciaValue);
begin
  if not Assigned(AResult) then
    Exit;
  if AResult = TGocciaUndefinedLiteralValue.UndefinedValue then
    Exit;
  WriteLn(AResult.ToStringLiteral.Value);
end;

function RunBare(const AOptions: TBareOptions): Integer;
var
  DisplayName: string;
  Engine: TGocciaEngine;
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

    Engine := TGocciaEngine.Create(DisplayName, Source, []);
    try
      ConfigureEngine(Engine, AOptions);
      try
        ScriptResult := Engine.Execute;
        PrintResult(ScriptResult.Result);
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
