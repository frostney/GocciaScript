program GocciaHeapSnapshotProbe;

{ Heap snapshot prototype host.

  An embedder that pauses on a host call and resumes later, possibly in
  another process. The entry module gets two globals: print(...) writes a
  line to standard output, and hostCall(name, arg) returns a pending promise
  the host settles once the engine is idle.

    GocciaHeapSnapshotProbe run FILE [ANSWER]
      Answers every host call with ANSWER (a number, default 3).
    GocciaHeapSnapshotProbe census FILE [ANSWER]
      As run, and at every pause writes a census of the live classes the
      script created to standard error.
    GocciaHeapSnapshotProbe snapshot FILE OUT
      Runs until the first pause, writes the snapshot to OUT and stops.
    GocciaHeapSnapshotProbe restore FILE IN [ANSWER]
      Recompiles FILE without running it, restores the snapshot IN, answers
      the pending host call and continues. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  Generics.Collections,
  SysUtils,

  TimingUtils,

  Goccia.Arguments.Collection,
  Goccia.Bytecode.Module,
  Goccia.Engine,
  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Census,
  Goccia.HeapSnapshot.Codec,
  Goccia.MicrotaskQueue,
  Goccia.Scope,
  Goccia.Values.Error,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.VM.Exception;

type
  TProbeMode = (pmRun, pmCensus, pmSnapshot, pmRestore);

  TProbeHost = class;

  TProbeExtension = class(TGocciaEngineExtension)
  public
    Host: TProbeHost;
    procedure WaitForIdle; override;
    function HasPendingWork: Boolean; override;
    procedure DiscardPending; override;
  end;

  ESnapshotTaken = class(Exception);

  TProbeHost = class
  private
    FMode: TProbeMode;
    FAnswer: Double;
    FSnapshotPath: string;
    FEngine: TGocciaEngine;
    FPending: TList<TGocciaPromiseValue>;
    FBaseline: TGocciaHeapBaseline;
    FPauseCount: Integer;
    FModuleScope: TGocciaScope;
    FModule: TObject;
    FBuiltins: TGocciaHeapSnapshotBuiltinTable;
    procedure AnswerAll;
    procedure TakeSnapshot;
    procedure RestoreSnapshot(const AModuleScope: TGocciaScope;
      const AModule: TObject);
  public
    constructor Create(const AMode: TProbeMode; const AAnswer: Double;
      const ASnapshotPath: string);
    destructor Destroy; override;
    function Print(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HostCall(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure EntryModuleReady(const AEngine: TGocciaEngine;
      const AModuleScope: TGocciaScope; const AModule: TObject;
      var ASkipBody: Boolean);
    procedure Pause;
    procedure Discard;
    property Pending: TList<TGocciaPromiseValue> read FPending;
    property Engine: TGocciaEngine read FEngine write FEngine;
    property Baseline: TGocciaHeapBaseline read FBaseline;
    property Builtins: TGocciaHeapSnapshotBuiltinTable read FBuiltins
      write FBuiltins;
  end;

{ TProbeExtension }

procedure TProbeExtension.WaitForIdle;
begin
  if Host.Pending.Count > 0 then
    Host.Pause;
end;

function TProbeExtension.HasPendingWork: Boolean;
begin
  Result := Host.Pending.Count > 0;
end;

procedure TProbeExtension.DiscardPending;
begin
  Host.Discard;
end;

{ TProbeHost }

constructor TProbeHost.Create(const AMode: TProbeMode; const AAnswer: Double;
  const ASnapshotPath: string);
begin
  inherited Create;
  FMode := AMode;
  FAnswer := AAnswer;
  FSnapshotPath := ASnapshotPath;
  FPending := TList<TGocciaPromiseValue>.Create;
end;

destructor TProbeHost.Destroy;
begin
  Discard;
  FPending.Free;
  FBaseline.Free;
  inherited;
end;

procedure TProbeHost.Discard;
var
  GC: TGarbageCollector;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    for I := 0 to FPending.Count - 1 do
      GC.RemoveTempRoot(FPending[I]);
  FPending.Clear;
end;

function TProbeHost.Print(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Line: string;
begin
  Line := '';
  for I := 0 to AArgs.Length - 1 do
  begin
    if I > 0 then
      Line := Line + ' ';
    Line := Line + AArgs.GetElement(I).ToStringLiteral.Value;
  end;
  WriteLn(Line);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TProbeHost.HostCall(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Promise);
  FPending.Add(Promise);
  Result := Promise;
end;

procedure TProbeHost.AnswerAll;
var
  Calls: array of TGocciaPromiseValue;
  GC: TGarbageCollector;
  I: Integer;
begin
  GC := TGarbageCollector.Instance;
  SetLength(Calls, FPending.Count);
  for I := 0 to FPending.Count - 1 do
    Calls[I] := FPending[I];
  FPending.Clear;
  for I := 0 to High(Calls) do
  begin
    Calls[I].Resolve(TGocciaNumberLiteralValue.Create(FAnswer));
    GC.RemoveTempRoot(Calls[I]);
  end;
end;

procedure TProbeHost.Pause;
var
  Census: TGocciaHeapCensus;
begin
  Inc(FPauseCount);
  case FMode of
    pmCensus:
      begin
        Census := TGocciaHeapCensus.Create;
        try
          Census.Take(FBaseline);
          WriteLn(ErrOutput, Census.ToJson(FEngine.SourcePath,
            'pause-' + IntToStr(FPauseCount)));
        finally
          Census.Free;
        end;
      end;
    pmSnapshot:
      begin
        TakeSnapshot;
        raise ESnapshotTaken.Create('snapshot taken');
      end;
  end;
  AnswerAll;
end;

procedure TProbeHost.EntryModuleReady(const AEngine: TGocciaEngine;
  const AModuleScope: TGocciaScope; const AModule: TObject;
  var ASkipBody: Boolean);
begin
  FModuleScope := AModuleScope;
  FModule := AModule;
  if FMode = pmRestore then
  begin
    RestoreSnapshot(AModuleScope, AModule);
    ASkipBody := True;
  end;
end;

procedure TProbeHost.TakeSnapshot;
var
  PendingValues: array of TGocciaValue;
  I: Integer;
  Writer: TGocciaHeapSnapshotWriter;
  Stream: TMemoryStream;
  StartNs, SerializeNs, AccountNs: Int64;
begin
  SetLength(PendingValues, FPending.Count);
  for I := 0 to FPending.Count - 1 do
    PendingValues[I] := FPending[I];
  Stream := TMemoryStream.Create;
  Writer := TGocciaHeapSnapshotWriter.Create(FBuiltins,
    TGocciaBytecodeModule(FModule), FModuleScope);
  try
    StartNs := GetNanoseconds;
    // Intrinsics the run created on first use (an array iterator's
    // prototype, for one) join the built-in table under their realm slot.
    ExtendBuiltinTableWithLateSlots(FBuiltins, FEngine, Writer.LateSlots);
    Writer.Serialize(PendingValues, Stream);
    SerializeNs := GetNanoseconds - StartNs;
    StartNs := GetNanoseconds;
    Writer.CheckAccounting(FBaseline);
    AccountNs := GetNanoseconds - StartNs;
    Stream.SaveToFile(FSnapshotPath);
    WriteLn(ErrOutput, Format(
      'snapshot: %d bytes, %d objects, %d builtin refs, serialize %.3f ms, accounting %.3f ms',
      [Stream.Size, Writer.ObjectCount, Writer.BuiltinRefCount,
       SerializeNs / 1e6, AccountNs / 1e6]));
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

procedure TProbeHost.RestoreSnapshot(const AModuleScope: TGocciaScope;
  const AModule: TObject);
var
  Reader: TGocciaHeapSnapshotReader;
  LateSlots: TGocciaLateSlotRestorer;
  Stream: TMemoryStream;
  Promises: TArray<TGocciaValue>;
  I: Integer;
  StartNs: Int64;
begin
  Stream := TMemoryStream.Create;
  Reader := TGocciaHeapSnapshotReader.Create(FBuiltins,
    TGocciaBytecodeModule(AModule), AModuleScope);
  LateSlots := TGocciaLateSlotRestorer.Create(FBuiltins, FEngine);
  try
    Reader.Context := FEngine.VM;
    Reader.LateSlotResolver := LateSlots.Resolve;
    Stream.LoadFromFile(FSnapshotPath);
    StartNs := GetNanoseconds;
    Promises := Reader.Restore(Stream);
    WriteLn(ErrOutput, Format('restore: %d bytes, %d objects, %.3f ms',
      [Stream.Size, Reader.ObjectCount, (GetNanoseconds - StartNs) / 1e6]));
    for I := 0 to High(Promises) do
    begin
      if not (Promises[I] is TGocciaPromiseValue) then
        raise EGocciaHeapSnapshotRefused.Create('a pending root is not a promise');
      TGarbageCollector.Instance.AddTempRoot(Promises[I]);
      FPending.Add(TGocciaPromiseValue(Promises[I]));
    end;
  finally
    LateSlots.Free;
    Reader.Free;
    Stream.Free;
  end;
end;

function ParseMode(const AText: string): TProbeMode;
begin
  if AText = 'run' then
    Result := pmRun
  else if AText = 'census' then
    Result := pmCensus
  else if AText = 'snapshot' then
    Result := pmSnapshot
  else if AText = 'restore' then
    Result := pmRestore
  else
    raise Exception.Create('Unknown mode: ' + AText);
end;

function RunProbe: Integer;
var
  Mode: TProbeMode;
  FileName, SnapshotPath: string;
  Answer: Double;
  Source: TStringList;
  Host: TProbeHost;
  Engine: TGocciaEngine;
  Extension: TProbeExtension;
  Builtins: TGocciaHeapSnapshotBuiltinTable;
  StartNs: Int64;
begin
  Result := 0;
  if ParamCount < 2 then
  begin
    WriteLn(ErrOutput,
      'Usage: GocciaHeapSnapshotProbe run|census FILE [ANSWER] | snapshot FILE OUT | restore FILE IN [ANSWER]');
    Exit(2);
  end;
  Mode := ParseMode(ParamStr(1));
  FileName := ParamStr(2);
  SnapshotPath := '';
  Answer := 3;
  if Mode in [pmSnapshot, pmRestore] then
  begin
    SnapshotPath := ParamStr(3);
    if ParamCount >= 4 then
      Answer := StrToFloat(ParamStr(4));
  end
  else if ParamCount >= 3 then
    Answer := StrToFloat(ParamStr(3));

  Source := TStringList.Create;
  Builtins := nil;
  try
    Source.LoadFromFile(FileName);
    Host := TProbeHost.Create(Mode, Answer, SnapshotPath);
    try
      Engine := TGocciaEngine.Create(FileName, Source);
      try
        Engine.SourceType := stModule;
        Host.Engine := Engine;
        Engine.RegisterGlobal('print',
          TGocciaNativeFunctionValue.CreateWithoutPrototype(Host.Print,
            'print', -1));
        Engine.RegisterGlobal('hostCall',
          TGocciaNativeFunctionValue.CreateWithoutPrototype(Host.HostCall,
            'hostCall', 2));
        Engine.RefreshGlobalThis;
        Extension := TProbeExtension.Create;
        Extension.Host := Host;
        Engine.AddExtension(Extension);
        Engine.OnEntryModuleReady := Host.EntryModuleReady;

        if Mode in [pmSnapshot, pmRestore] then
        begin
          StartNs := GetNanoseconds;
          Builtins := TGocciaHeapSnapshotBuiltinTable.Create;
          BuildBuiltinTable(Builtins, Engine);
          Host.Builtins := Builtins;
          WriteLn(ErrOutput, Format('builtin table: %d objects, %.3f ms',
            [Builtins.Count, (GetNanoseconds - StartNs) / 1e6]));
        end;
        if Mode in [pmCensus, pmSnapshot] then
        begin
          Host.FBaseline := TGocciaHeapBaseline.Create;
          Host.FBaseline.Capture;
        end;

        try
          Engine.Execute;
        except
          on E: ESnapshotTaken do
            Result := 0;
          on E: EGocciaBytecodeThrow do
          begin
            WriteLn(ErrOutput, 'Uncaught: ',
              E.ThrownValue.ToStringLiteral.Value);
            Result := 1;
          end;
          on E: TGocciaThrowValue do
          begin
            WriteLn(ErrOutput, 'Uncaught: ', E.Value.ToStringLiteral.Value);
            Result := 1;
          end;
        end;
      finally
        Host.Discard;
        Engine.Free;
      end;
    finally
      Host.Free;
    end;
  finally
    Builtins.Free;
    Source.Free;
  end;
end;

begin
  try
    ExitCode := RunProbe;
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'Error: ', E.ClassName, ': ', E.Message);
      if GetEnvironmentVariable('GOCCIA_HEAP_SNAPSHOT_TRACE') <> '' then
        DumpExceptionBackTrace(ErrOutput);
      ExitCode := 1;
    end;
  end;
end.
