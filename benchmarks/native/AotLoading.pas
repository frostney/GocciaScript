program AotLoading;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TimingUtils,

  Goccia.AST.Node,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Module,
  Goccia.Bytecode.Profile,
  Goccia.Bytecode.Profile.Options,
  Goccia.Compiler,
  Goccia.FloatingPoint,
  Goccia.SourcePipeline;

const
  SAMPLE_COUNT = 11;
  WARMUP_COUNT = 3;

type
  TSamples = array[0..SAMPLE_COUNT - 1] of Int64;

function MedianMilliseconds(var AValues: TSamples): Double;
var
  I, J: Integer;
  Value: Int64;
begin
  for I := 1 to High(AValues) do
  begin
    Value := AValues[I];
    J := I - 1;
    while (J >= 0) and (AValues[J] > Value) do
    begin
      AValues[J + 1] := AValues[J];
      Dec(J);
    end;
    AValues[J + 1] := Value;
  end;
  Result := AValues[SAMPLE_COUNT div 2] / 1000000;
end;

procedure Measure(const AFunctionCount: Integer);
var
  Source: TStringList;
  Options: TGocciaSourcePipelineOptions;
  ExpectedProfile: TGocciaCompileProfile;
  Parsed: TGocciaSourcePipelineResult;
  Compiler: TGocciaCompiler;
  Module, Loaded: TGocciaBytecodeModule;
  LinkProgram: TGocciaProgram;
  Bytes: TBytes;
  I, Trial: Integer;
  Started, CompileTime, LoadTime: Int64;
  CompileSamples, LoadSamples: TSamples;
  CompileMilliseconds, LoadMilliseconds: Double;
begin
  Source := TStringList.Create;
  try
    for I := 1 to AFunctionCount do
      Source.Add(Format(
        'export const function%d = (value) => { const shifted = value + %d; return shifted * 3 + 1; };',
        [I, I]));
    Options := TGocciaSourcePipeline.DefaultOptions;
    Options.SourceType := stModule;
    ExpectedProfile := BuildBytecodeCompileProfile(Options, False);
    for Trial := -WARMUP_COUNT to SAMPLE_COUNT - 1 do
    begin
      Started := GetNanoseconds;
      Parsed := TGocciaSourcePipeline.Parse(Source, 'aot-library.mjs', Options);
      Compiler := TGocciaCompiler.Create('aot-library.mjs');
      try
        Compiler.GlobalBackedTopLevel := True;
        Compiler.PreinitializedTopLevelFunctions := True;
        Module := Compiler.Compile(Parsed.ProgramNode);
        Module.CompileProfile := ExpectedProfile;
        CompileTime := GetNanoseconds - Started;
      finally
        Compiler.Free;
        Parsed.Free;
      end;
      try
        // Serialization happens once and is never charged to either load path.
        if Trial = -WARMUP_COUNT then
          Bytes := SaveModuleToBuffer(Module);
      finally
        Module.Free;
      end;

      Started := GetNanoseconds;
      Loaded := LoadModuleFromBuffer(Bytes);
      try
        if not Loaded.CompileProfile.Matches(ExpectedProfile) then
          raise Exception.Create('Benchmark artifact profile mismatch');
        LinkProgram := Loaded.LinkPlan.Materialize;
        LoadTime := GetNanoseconds - Started;
        LinkProgram.Free;
      finally
        Loaded.Free;
      end;
      if Trial >= 0 then
      begin
        CompileSamples[Trial] := CompileTime;
        LoadSamples[Trial] := LoadTime;
      end;
    end;
    CompileMilliseconds := MedianMilliseconds(CompileSamples);
    LoadMilliseconds := MedianMilliseconds(LoadSamples);
    WriteLn('functions=', AFunctionCount, ' source_bytes=', Length(Source.Text),
      ' bytecode_bytes=', Length(Bytes), ' samples=', SAMPLE_COUNT,
      ' warmups=', WARMUP_COUNT);
    WriteLn('source_frontend_ms=', CompileMilliseconds:0:3,
      ' buffer_frontend_ms=', LoadMilliseconds:0:3,
      ' saving_pct=', (1 - LoadMilliseconds / CompileMilliseconds) * 100:0:2);
  finally
    Source.Free;
  end;
end;

var
  FloatingPointState: TGocciaFloatingPointState;
begin
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Measure(1000);
    Measure(5000);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end.
