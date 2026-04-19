program GocciaBundler;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Application,
  Goccia.AST.Node,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.CLI.Application,
  CLI.Options,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.FileExtensions,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.ScriptLoader.Input,
  Goccia.SourceMap,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Threading.Init,
  Goccia.Token,

  FileUtils in 'units/FileUtils.pas';

type
  TBundlerApp = class(TGocciaCLIApplication)
  private
    FOutputPath: TGocciaStringOption;
    FSourceMap: TGocciaStringOption;
    FASI: TGocciaFlagOption;

    function ParseSource(const ASource: TStringList; const AFileName: string;
      out ALexTimeNanoseconds, AParseTimeNanoseconds: Int64;
      out ASourceMap: TGocciaSourceMap): TGocciaProgram;
    function CompileSource(const ASource: TStringList;
      const AFileName: string): TGocciaBytecodeModule;
    procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
      const AFileName: string);
    procedure EmitBytecode(const ASource: TStringList; const AFileName,
      AOutputPath: string);
    function ResolveOutputPath(const AInputFile: string): string;
    procedure EmitFromFile(const AFileName: string);
    procedure EmitFromStdin;
    procedure EmitWorkerProc(const AFileName: string; const AIndex: Integer;
      out AConsoleOutput: string; out AErrorMessage: string; AData: Pointer);
    procedure EmitParallel(const AFiles: TStringList;
      const AJobCount: Integer);
    procedure EmitPath(const APath: string);
  protected
    procedure Configure; override;
    function UsageLine: string; override;
    procedure Validate; override;
    procedure ExecuteWithPaths(const APaths: TStringList); override;
  end;

{ TBundlerApp - Configure }

function TBundlerApp.UsageLine: string;
begin
  Result := '[file|directory|-] [options]';
end;

procedure TBundlerApp.Configure;
begin
  FASI := AddFlag('asi', 'Enable automatic semicolon insertion');
  FOutputPath := AddString('output',
    'Output path (single file) or output directory (multiple files)');
  FSourceMap := AddString('source-map',
    'Write a .map source map file (optional: explicit path)');
end;

{ TBundlerApp - Validate }

procedure TBundlerApp.Validate;
begin
  inherited Validate;
end;

{ TBundlerApp - Core logic }

function TBundlerApp.ParseSource(const ASource: TStringList;
  const AFileName: string;
  out ALexTimeNanoseconds, AParseTimeNanoseconds: Int64;
  out ASourceMap: TGocciaSourceMap): TGocciaProgram;
var
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  Warning: TGocciaParserWarning;
  StartTime, LexEnd, ParseEnd: Int64;
  OrigLine, OrigCol, I: Integer;
begin
  StartTime := GetNanoseconds;
  SourceText := StringListToLFText(ASource);

  ASourceMap := nil;
  if ppJSX in TGocciaEngine.DefaultPreprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    ASourceMap := JSXResult.SourceMap;
    if Assigned(ASourceMap) then
      ASourceMap.SetSourceContent(0, StringListToLFText(ASource));
  end;

  try
    Lexer := TGocciaLexer.Create(SourceText, AFileName);
    try
      Tokens := Lexer.ScanTokens;
      LexEnd := GetNanoseconds;
      ALexTimeNanoseconds := LexEnd - StartTime;

      Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
      Parser.AutomaticSemicolonInsertion := FASI.Present;
      try
        Result := Parser.Parse;
        ParseEnd := GetNanoseconds;
        AParseTimeNanoseconds := ParseEnd - LexEnd;

        if not GIsWorkerThread then
          for I := 0 to Parser.WarningCount - 1 do
          begin
            Warning := Parser.GetWarning(I);
            WriteLn(SysUtils.Format('Warning: %s', [Warning.Message]));
            if Warning.Suggestion <> '' then
              WriteLn(SysUtils.Format('  Suggestion: %s', [Warning.Suggestion]));
            if Assigned(ASourceMap) and
               ASourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
              WriteLn(SysUtils.Format('  --> %s:%d:%d', [AFileName, OrigLine, OrigCol]))
            else
              WriteLn(SysUtils.Format('  --> %s:%d:%d',
                [AFileName, Warning.Line, Warning.Column]));
          end;
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
    end;
  except
    ASourceMap.Free;
    ASourceMap := nil;
    raise;
  end;
end;

procedure TBundlerApp.WriteSourceMapIfEnabled(
  const ASourceMap: TGocciaSourceMap; const AFileName: string);
var
  MapOutputPath, SourceName: string;
begin
  if not FSourceMap.Present then
    Exit;
  if not Assigned(ASourceMap) then
    Exit;
  MapOutputPath := FSourceMap.ValueOr('');
  if MapOutputPath = '' then
  begin
    if AFileName = STDIN_FILE_NAME then
      MapOutputPath := ResolveOutputPath(AFileName) + EXT_MAP
    else
      MapOutputPath := AFileName + EXT_MAP;
  end;
  SourceName := AFileName;
  if (SourceName = STDIN_FILE_NAME) and FOutputPath.Present then
    SourceName := FOutputPath.Value;
  ASourceMap.FileName := ExtractFileName(SourceName);
  ASourceMap.SetSourcePath(0, ExtractFileName(SourceName));
  ASourceMap.SaveToFile(MapOutputPath);
  if not GIsWorkerThread then
    WriteLn(SysUtils.Format('  Source map written to %s', [MapOutputPath]));
end;

function TBundlerApp.CompileSource(const ASource: TStringList;
  const AFileName: string): TGocciaBytecodeModule;
var
  ProgramNode: TGocciaProgram;
  Compiler: TGocciaCompiler;
  CompiledModule: TGocciaBytecodeModule;
  LexTimeNanoseconds, ParseTimeNanoseconds: Int64;
  SourceMap: TGocciaSourceMap;
begin
  CompiledModule := nil;
  ProgramNode := ParseSource(ASource, AFileName,
    LexTimeNanoseconds, ParseTimeNanoseconds, SourceMap);
  try
    Compiler := TGocciaCompiler.Create(AFileName);
    try
      CompiledModule := Compiler.Compile(ProgramNode);
      WriteSourceMapIfEnabled(SourceMap, AFileName);
      Result := CompiledModule;
      CompiledModule := nil;
    finally
      Compiler.Free;
    end;
  finally
    ProgramNode.Free;
    SourceMap.Free;
    CompiledModule.Free;
  end;
end;

procedure TBundlerApp.EmitBytecode(const ASource: TStringList;
  const AFileName, AOutputPath: string);
var
  Module: TGocciaBytecodeModule;
  StartTime, EndTime: Int64;
begin
  if not GIsWorkerThread then
    WriteLn('Compiling: ', AFileName);
  StartTime := GetNanoseconds;

  Module := CompileSource(ASource, AFileName);
  try
    Goccia.Bytecode.Binary.SaveModuleToFile(Module, AOutputPath);
    EndTime := GetNanoseconds;
    if not GIsWorkerThread then
      WriteLn(SysUtils.Format('  Compiled to %s (%s)',
        [AOutputPath, FormatDuration(EndTime - StartTime)]));
  finally
    Module.Free;
  end;
end;

function TBundlerApp.ResolveOutputPath(const AInputFile: string): string;
begin
  if FOutputPath.Present then
  begin
    if DirectoryExists(FOutputPath.Value) then
      Result := IncludeTrailingPathDelimiter(FOutputPath.Value) +
        ChangeFileExt(ExtractFileName(AInputFile), EXT_GBC)
    else
      Result := FOutputPath.Value;
  end
  else
    Result := ChangeFileExt(AInputFile, EXT_GBC);
end;

procedure TBundlerApp.EmitFromFile(const AFileName: string);
var
  Source: TStringList;
  OutputPath: string;
begin
  if LowerCase(ExtractFileExt(AFileName)) = EXT_GBC then
    raise Exception.CreateFmt('Cannot compile bytecode file: %s', [AFileName]);

  OutputPath := ResolveOutputPath(AFileName);

  Source := ReadUTF8FileLines(AFileName);
  try
    EmitBytecode(Source, AFileName, OutputPath);
  finally
    Source.Free;
  end;
end;

procedure TBundlerApp.EmitFromStdin;
var
  Source: TStringList;
begin
  if not FOutputPath.Present then
    raise TGocciaParseError.Create(
      '--output=<path> is required when compiling from stdin.');

  if DirectoryExists(FOutputPath.Value) then
    raise TGocciaParseError.Create(
      '--output must be a file when compiling from stdin.');

  Source := ReadSourceFromText(Input);
  try
    EmitBytecode(Source, STDIN_FILE_NAME, FOutputPath.Value);
  finally
    Source.Free;
  end;
end;

procedure TBundlerApp.EmitWorkerProc(const AFileName: string;
  const AIndex: Integer; out AConsoleOutput: string;
  out AErrorMessage: string; AData: Pointer);
begin
  AConsoleOutput := '';
  AErrorMessage := '';
  try
    EmitFromFile(AFileName);
  except
    on E: Exception do
      AErrorMessage := E.Message;
  end;
end;

procedure TBundlerApp.EmitParallel(const AFiles: TStringList;
  const AJobCount: Integer);
var
  Pool: TGocciaThreadPool;
  I: Integer;
begin
  EnsureSharedPrototypesInitialized(EffectiveBuiltins);

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
    Pool.RunAll(AFiles, EmitWorkerProc);

    for I := 0 to AFiles.Count - 1 do
      if Pool.Results[I].ErrorMessage <> '' then
      begin
        WriteLn('Error in ', Pool.Results[I].FileName, ': ', Pool.Results[I].ErrorMessage);
        ExitCode := 1;
      end;
  finally
    Pool.Free;
  end;
end;

procedure TBundlerApp.EmitPath(const APath: string);
var
  Files: TStringList;
  I: Integer;
begin
  if IsStdinPath(APath) then
  begin
    EmitFromStdin;
    Exit;
  end;

  if DirectoryExists(APath) then
  begin
    Files := FindAllFiles(APath, ScriptExtensions);
    try
      if GetJobCount(Files.Count) > 1 then
      begin
        WriteLn(SysUtils.Format('Compiling %d files with %d workers',
          [Files.Count, GetJobCount(Files.Count)]));
        EmitParallel(Files, GetJobCount(Files.Count));
      end
      else
        for I := 0 to Files.Count - 1 do
        begin
          if I > 0 then
            WriteLn;
          EmitFromFile(Files[I]);
        end;
    finally
      Files.Free;
    end;
  end
  else if FileExists(APath) then
    EmitFromFile(APath)
  else
    raise Exception.Create('Path not found: ' + APath);
end;

{ TBundlerApp - ExecuteWithPaths }

procedure TBundlerApp.ExecuteWithPaths(const APaths: TStringList);
var
  I: Integer;
begin
  if FOutputPath.Present and (FOutputPath.Value <> '') and
     not DirectoryExists(FOutputPath.Value) and
     ((APaths.Count > 1) or
      ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TGocciaParseError.Create(
      '--output must be a directory when compiling multiple files.');

  if (FSourceMap.ValueOr('') <> '') and
     ((APaths.Count > 1) or
      ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TGocciaParseError.Create(
      '--source-map=<file> supports a single input file or stdin.');

  if APaths.Count = 0 then
    EmitFromStdin
  else
    for I := 0 to APaths.Count - 1 do
    begin
      if I > 0 then
        WriteLn;
      EmitPath(APaths[I]);
    end;
end;

{ Entry point }

var
  RunResult: Integer;
begin
  RunResult := TGocciaApplication.RunApplication(TBundlerApp, 'GocciaBundler');
  if RunResult <> 0 then
    ExitCode := RunResult;
end.
