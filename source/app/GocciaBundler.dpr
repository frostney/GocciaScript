program GocciaBundler;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TimingUtils,

  Goccia.Application,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  CLI.ConfigFile,
  Goccia.CLI.Application,
  Goccia.CLI.SourceMaps,
  Goccia.CLI.SourcePipelineResult,
  Goccia.CLI.Options,
  CLI.Options,
  Goccia.Compiler,
  Goccia.Engine,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.ScriptLoader.Input,
  Goccia.SourcePipeline,
  Goccia.SourceMap,
  Goccia.TextFiles,
  Goccia.Threading,
  Goccia.Threading.Flags,
  Goccia.Threading.Init,

  FileUtils in 'units/FileUtils.pas';

type
  TBundlerApp = class(TGocciaCLIApplication)
  private
    FOutputPath: TStringOption;
    FSourceMap: TStringOption;

    function CompileSource(const ASource: TStringList;
      const AFileName: string;
      out ASourceMap: TGocciaSourceMap): TGocciaBytecodeModule;
    procedure WriteSourceMapIfEnabled(const ASourceMap: TGocciaSourceMap;
      const AFileName, AOutputPath: string);
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
  AddEngineOptions;
  FOutputPath := AddString('output',
    'Output path (single file) or output directory (multiple files)');
  FSourceMap := TStringOption(Add(TOptionalStringOption.Create('source-map',
    'Write a .map source map file (optional: explicit path)')));
end;

{ TBundlerApp - Validate }

procedure TBundlerApp.Validate;
begin
  inherited Validate;
end;

{ TBundlerApp - Core logic }

procedure TBundlerApp.WriteSourceMapIfEnabled(
  const ASourceMap: TGocciaSourceMap; const AFileName, AOutputPath: string);
var
  MapOutputPath, SourceName: string;
begin
  if not FSourceMap.Present then
    Exit;
  MapOutputPath := FSourceMap.ValueOr('');
  if MapOutputPath = '' then
    MapOutputPath := ChangeFileExt(AOutputPath, EXT_MAP);
  SourceName := AFileName;
  if (SourceName = STDIN_FILE_NAME) and FOutputPath.Present then
    SourceName := AOutputPath;
  WriteSourceMapIfAvailable(ASourceMap, MapOutputPath, AOutputPath, SourceName,
    not GIsWorkerThread);
end;

function TBundlerApp.CompileSource(const ASource: TStringList;
  const AFileName: string;
  out ASourceMap: TGocciaSourceMap): TGocciaBytecodeModule;
var
  SourcePipelineResult: TGocciaCLISourcePipelineResult;
  Compiler: TGocciaCompiler;
  CompiledModule: TGocciaBytecodeModule;
  FileConfig: TConfigEntryArray;
  EffectiveStrictTypes: Boolean;
  EffectiveSourceType: TGocciaSourceType;
  EffectiveCompatibility: TGocciaCompatibilityFlags;
  PipelineOptions: TGocciaSourcePipelineOptions;
begin
  ASourceMap := nil;
  { Resolve source pipeline flags: CLI flag > per-file config >
    root config > default. }
  FileConfig := DiscoverFileConfig(AFileName);
  EffectiveCompatibility := ResolveCompatibilityFlags(
    EngineOptions, FileConfig);
  EffectiveSourceType := ResolveSourceTypeOption(EngineOptions.SourceType,
    FileConfig, AFileName);
  EffectiveStrictTypes := ResolveFlagOption(
    EngineOptions.StrictTypes, FileConfig);

  CompiledModule := nil;
  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := TGocciaEngine.DefaultPreprocessors;
  PipelineOptions.Compatibility := EffectiveCompatibility;
  PipelineOptions.SourceType := EffectiveSourceType;
  SourcePipelineResult := TGocciaCLISourcePipelineResult.Parse(ASource, AFileName,
    PipelineOptions, False);
  try
    Compiler := TGocciaCompiler.Create(AFileName);
    try
      Compiler.StrictTypes := EffectiveStrictTypes;
      Compiler.NonStrictMode := (cfNonStrictMode in EffectiveCompatibility) and
        (EffectiveSourceType = stScript);
      CompiledModule := Compiler.Compile(SourcePipelineResult.ProgramNode);
      ASourceMap := SourcePipelineResult.TakeSourceMap;
      Result := CompiledModule;
      CompiledModule := nil;
    finally
      Compiler.Free;
    end;
  finally
    SourcePipelineResult.Free;
    CompiledModule.Free;
  end;
end;

procedure TBundlerApp.EmitBytecode(const ASource: TStringList;
  const AFileName, AOutputPath: string);
var
  Module: TGocciaBytecodeModule;
  SourceMap: TGocciaSourceMap;
  StartTime, EndTime: Int64;
begin
  if not GIsWorkerThread then
    WriteLn('Compiling: ', AFileName);
  StartTime := GetNanoseconds;

  SourceMap := nil;
  Module := CompileSource(ASource, AFileName, SourceMap);
  try
    Goccia.Bytecode.Binary.SaveModuleToFile(Module, AOutputPath);
    WriteSourceMapIfEnabled(SourceMap, AFileName, AOutputPath);
    EndTime := GetNanoseconds;
    if not GIsWorkerThread then
      WriteLn(SysUtils.Format('  Compiled to %s (%s)',
        [AOutputPath, FormatDuration(EndTime - StartTime)]));
  finally
    SourceMap.Free;
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

  Source := SourceRegistry.Load(AFileName);
  try
    EmitBytecode(Source, AFileName, OutputPath);
  finally
    Source.Free;
  end;
end;

{ Replace characters that are illegal in filenames on Windows (< > : " /
  \ | * ?) and ASCII control characters with '_'.  Used when synthesizing
  on-disk paths from multifile section names whose synthetic basename
  contains the angle brackets of the stdin marker (e.g.
  '<stdin>[part1]').  The section name itself stays intact for diagnostic
  output; only the disk path is sanitized. }
function SanitizeFileBaseName(const ABaseName: string): string;
var
  I: Integer;
  C: Char;
begin
  SetLength(Result, Length(ABaseName));
  for I := 1 to Length(ABaseName) do
  begin
    C := ABaseName[I];
    if (C = '<') or (C = '>') or (C = ':') or (C = '"') or
       (C = '/') or (C = '\') or (C = '|') or (C = '?') or (C = '*') or
       (Ord(C) < 32) then
      Result[I] := '_'
    else
      Result[I] := C;
  end;
end;

procedure TBundlerApp.EmitFromStdin;
var
  Source, SectionSource, Names: TStringList;
  OutputPath: string;
  I: Integer;
begin
  if not FOutputPath.Present then
    raise TParseError.Create(
      '--output=<path> is required when compiling from stdin.');

  Source := ReadSourceFromText(Input);

  if MultifileEnabled then
  begin
    if not DirectoryExists(FOutputPath.Value) then
      raise TParseError.Create(
        '--output must be a directory when --multifile is set with stdin.');

    // Ownership of Source transfers to SplitStdinMultifile.
    Names := SplitStdinMultifile(Source);
    try
      for I := 0 to Names.Count - 1 do
      begin
        // Sanitize the filename so '<stdin>[partN]' becomes a Windows-
        // legal '_stdin_[partN].gbc' on disk.  The section name stays
        // intact for EmitBytecode (used for diagnostics).
        OutputPath := IncludeTrailingPathDelimiter(FOutputPath.Value) +
          ChangeFileExt(SanitizeFileBaseName(ExtractFileName(Names[I])),
            EXT_GBC);
        SectionSource := SourceRegistry.Load(Names[I]);
        try
          EmitBytecode(SectionSource, Names[I], OutputPath);
        finally
          SectionSource.Free;
        end;
      end;
    finally
      Names.Free;
    end;
    Exit;
  end;

  if DirectoryExists(FOutputPath.Value) then
    raise TParseError.Create(
      '--output must be a file when compiling from stdin.');

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
  EnsureSharedPrototypesInitialized;

  Pool := TGocciaThreadPool.Create(AJobCount);
  try
    if Assigned(TGarbageCollector.Instance) then
      Pool.MaxBytes := TGarbageCollector.Instance.MaxBytes;
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
  Files, RawFiles, SinglePath: TStringList;
  I: Integer;
begin
  if IsStdinPath(APath) then
  begin
    EmitFromStdin;
    Exit;
  end;

  if DirectoryExists(APath) then
  begin
    RawFiles := FindAllFiles(APath, ScriptExtensions);
    try
      Files := ExpandMultifileFiles(RawFiles);
    finally
      RawFiles.Free;
    end;
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
  begin
    if MultifileEnabled then
    begin
      SinglePath := TStringList.Create;
      try
        SinglePath.Add(APath);
        Files := ExpandMultifileFiles(SinglePath);
      finally
        SinglePath.Free;
      end;
      try
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
    else
      EmitFromFile(APath);
  end
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
    raise TParseError.Create(
      '--output must be a directory when compiling multiple files.');

  if MultifileEnabled and FOutputPath.Present and
     (FOutputPath.Value <> '') and not DirectoryExists(FOutputPath.Value) then
    raise TParseError.Create(
      '--output=<file> cannot be combined with --multifile (an input '
      + 'may expand to multiple sections); pass a directory or omit '
      + '--output.');

  if (FSourceMap.ValueOr('') <> '') and
     ((APaths.Count > 1) or
      ((APaths.Count = 1) and DirectoryExists(APaths[0]))) then
    raise TParseError.Create(
      '--source-map=<file> supports a single input file or stdin.');

  // Use Present rather than the value to catch the bare --source-map
  // form too — see ScriptLoader for the rationale.
  if FSourceMap.Present and MultifileEnabled then
    raise TParseError.Create(
      '--source-map cannot be combined with --multifile (an input '
      + 'may expand to multiple sections).');

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
