unit Goccia.CLI.ParsedSource;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.AST.Node,
  Goccia.SourceMap,
  Goccia.SourcePipeline;

type
  TGocciaCLIParsedSource = class
  private
    FProgramNode: TGocciaProgram;
    FSourceMap: TGocciaSourceMap;
    FGeneratedSourceLines: TStringList;
    FLexTimeNanoseconds: Int64;
    FParseTimeNanoseconds: Int64;
  public
    destructor Destroy; override;

    class function Parse(const ASource: TStringList; const AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions;
      const ASuppressWarnings: Boolean): TGocciaCLIParsedSource; static;

    procedure RegisterCoverageSource(const AFileName: string);

    property ProgramNode: TGocciaProgram read FProgramNode;
    property SourceMap: TGocciaSourceMap read FSourceMap;
    property GeneratedSourceLines: TStringList read FGeneratedSourceLines;
    property LexTimeNanoseconds: Int64 read FLexTimeNanoseconds;
    property ParseTimeNanoseconds: Int64 read FParseTimeNanoseconds;
  end;

procedure WriteCLISourceMapIfRequested(const ASourceMap: TGocciaSourceMap;
  const ARequested: Boolean; const AOutputPath, ASourceName: string;
  const APrintMessage: Boolean);

implementation

uses
  SysUtils,

  Goccia.Coverage,
  Goccia.Threading;

procedure PrintPipelineWarnings(const AFileName: string;
  const APipelineResult: TGocciaSourcePipelineResult;
  const ASuppressWarnings: Boolean);
var
  I: Integer;
  Warning: TGocciaSourcePipelineWarning;
begin
  if ASuppressWarnings or GIsWorkerThread then
    Exit;

  for I := 0 to APipelineResult.WarningCount - 1 do
  begin
    Warning := APipelineResult.Warnings[I];
    WriteLn(SysUtils.Format('Warning: %s', [Warning.Message]));
    if Warning.Suggestion <> '' then
      WriteLn(SysUtils.Format('  Suggestion: %s', [Warning.Suggestion]));
    WriteLn(SysUtils.Format('  --> %s:%d:%d',
      [AFileName, Warning.Line, Warning.Column]));
  end;
end;

procedure WriteCLISourceMapIfRequested(const ASourceMap: TGocciaSourceMap;
  const ARequested: Boolean; const AOutputPath, ASourceName: string;
  const APrintMessage: Boolean);
begin
  if not ARequested then
    Exit;
  if not Assigned(ASourceMap) then
    Exit;

  ASourceMap.FileName := ExtractFileName(ASourceName);
  ASourceMap.SetSourcePath(0, ExtractFileName(ASourceName));
  ASourceMap.SaveToFile(AOutputPath);

  if APrintMessage then
    WriteLn(SysUtils.Format('  Source map written to %s', [AOutputPath]));
end;

destructor TGocciaCLIParsedSource.Destroy;
begin
  FGeneratedSourceLines.Free;
  FSourceMap.Free;
  FProgramNode.Free;
  inherited Destroy;
end;

class function TGocciaCLIParsedSource.Parse(const ASource: TStringList;
  const AFileName: string; const AOptions: TGocciaSourcePipelineOptions;
  const ASuppressWarnings: Boolean): TGocciaCLIParsedSource;
var
  PipelineResult: TGocciaSourcePipelineResult;
begin
  Result := TGocciaCLIParsedSource.Create;
  try
    PipelineResult := TGocciaSourcePipeline.Parse(ASource, AFileName,
      AOptions);
    try
      Result.FLexTimeNanoseconds := PipelineResult.LexTimeNanoseconds;
      Result.FParseTimeNanoseconds := PipelineResult.ParseTimeNanoseconds;
      PrintPipelineWarnings(AFileName, PipelineResult, ASuppressWarnings);

      Result.FProgramNode := PipelineResult.TakeProgramNode;
      Result.FSourceMap := PipelineResult.TakeSourceMap;
      Result.FGeneratedSourceLines := PipelineResult.TakeGeneratedSourceLines;
    finally
      PipelineResult.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TGocciaCLIParsedSource.RegisterCoverageSource(
  const AFileName: string);
begin
  if not Assigned(TGocciaCoverageTracker.Instance) then
    Exit;
  if not TGocciaCoverageTracker.Instance.Enabled then
    Exit;
  if not Assigned(FGeneratedSourceLines) then
    Exit;

  TGocciaCoverageTracker.Instance.RegisterSourceFile(
    AFileName, CountExecutableLines(FGeneratedSourceLines));
  if Assigned(FSourceMap) then
    TGocciaCoverageTracker.Instance.RegisterSourceMap(
      AFileName, FSourceMap.Clone);
end;

end.
