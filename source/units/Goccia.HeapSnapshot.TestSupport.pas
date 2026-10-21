unit Goccia.HeapSnapshot.TestSupport;

{$I Goccia.inc}

{ Helpers shared by the heap snapshot test programs (ADR 0148): write what a
  script or a module hands over, restore it in a fresh engine and check it
  there by script, or report the refusal. Each check runs as global eval code
  with the restored value in `restored`. }

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.HeapSnapshot.Codec,
  Goccia.Values.Primitives;

const
  TEST_PATH = '<heap-snapshot-test>';

type
  { capture(value): hands a module's value to the test, rooted. }
  TCapture = class
  public
    Value: TGocciaValue;
    function Capture(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

// Evaluates ASource as global eval code in a fresh engine and writes the
// value it returns; ARefusal is the refusal's message, or '' when the
// snapshot was taken.
function Snapshot(const ASource: string; out ARefusal: string;
  const AAccounting: Boolean = False): TBytes;
function RestoreAndCheck(const ABytes: TBytes; const ACheck: string;
  const ALimits: TGocciaHeapSnapshotLimits): string; overload;
function RestoreAndCheck(const ABytes: TBytes;
  const ACheck: string): string; overload;
// The refusal's message, or '' when the input restored.
function RestoreRefusal(const ABytes: TBytes;
  const ALimits: TGocciaHeapSnapshotLimits): string; overload;
function RestoreRefusal(const ABytes: TBytes): string; overload;
// In an engine the caller keeps for many restores.
function RestoreRefusalIn(const AEngine: TGocciaEngine;
  const ABytes: TBytes): string;
function RoundTrip(const ASource, ACheck: string): string;
// Runs ASource as a script in a fresh engine and writes what it passed to
// capture() (as a module when AAsModule). AWithEval installs the test host's
// realm-bound eval without changing the engine's language options.
function SnapshotModule(const ASource: string; out ARefusal: string;
  const AAsModule: Boolean = False; const AWithEval: Boolean = False): TBytes;
// Runs ASource as a script in a fresh engine, snapshots what it passed to
// capture(), then runs ASource again in another engine (so that engine
// has compiled the same module) and restores there.
function RoundTripModule(const ASource, ACheck: string;
  const AWithEval: Boolean = False): string;
// Restores ABytes, which ASource's run wrote, in an engine that ran
// ASource too, and checks it as RoundTripModule does.
function RoundTripModuleBytes(const ASource: string; const ABytes: TBytes;
  const ACheck: string; const AWithEval: Boolean = False): string;
// The position of object AIndex's entry (its codec, then its payload's
// length and the payload) in a snapshot.
function ObjectEntryAt(const ABytes: TBytes; const AIndex: Integer): Integer;

implementation

uses
  Classes,

  TestingPascalLibrary,

  Goccia.GarbageCollector,
  Goccia.HeapSnapshot,
  Goccia.Scope.BindingMap,
  Goccia.SourcePipeline,
  Goccia.Test262.Host,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue;

function InstallSnapshotEval(const AEngine: TGocciaEngine): TGocciaTest262Host;
var
  Options: TGocciaTest262EngineOptions;
begin
  Options := Default(TGocciaTest262EngineOptions);
  Options.Compatibility := AEngine.Compatibility;
  Options.SourceType := AEngine.SourceType;
  Result := TGocciaTest262Host.Create(Options);
  try
    Result.Install(AEngine);
  except
    Result.Free;
    raise;
  end;
end;

{ TCapture }

function TCapture.Capture(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(Value) then
    TGarbageCollector.Instance.RemoveTempRoot(Value);
  Value := AArgs.GetElement(0);
  TGarbageCollector.Instance.AddTempRoot(Value);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;
function Snapshot(const ASource: string;
  out ARefusal: string; const AAccounting: Boolean): TBytes;
var
  Engine: TGocciaEngine;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Stage: TGocciaGlobalEvalStage;
  Value: TGocciaValue;
  Stream: TBytesStream;
begin
  Result := nil;
  ARefusal := '';
  Lines := TStringList.Create;
  Engine := nil;
  Session := nil;
  Stream := TBytesStream.Create;
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Session := TGocciaHeapSnapshotSession.Create(Engine);
    Session.CheckAccounting := AAccounting;
    Value := Engine.EvaluateGlobalEval(ASource, TEST_PATH, Stage);
    TGarbageCollector.Instance.AddTempRoot(Value);
    try
      Session.AddToBaseline(Value);
      try
        Session.Snapshot([Value], Stream);
        Result := Copy(Stream.Bytes, 0, Stream.Size);
      except
        on E: EGocciaHeapSnapshotRefused do
          ARefusal := E.Message;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Value);
    end;
  finally
    Stream.Free;
    Session.Free;
    Engine.Free;
    Lines.Free;
  end;
end;

function RestoreAndCheck(const ABytes: TBytes;
  const ACheck: string; const ALimits: TGocciaHeapSnapshotLimits): string;
var
  Engine: TGocciaEngine;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Stream: TBytesStream;
  Stage: TGocciaGlobalEvalStage;
begin
  Lines := TStringList.Create;
  Engine := nil;
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream, ALimits);
    TGocciaObjectValue(Engine.Realm.GlobalObject).AssignProperty('restored',
      TGocciaValue(Restore.Roots[0]));
    Result := Engine.EvaluateGlobalEval(ACheck, TEST_PATH,
      Stage).ToStringLiteral.Value;
  finally
    Restore.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

function RestoreAndCheck(const ABytes: TBytes;
  const ACheck: string): string;
begin
  Result := RestoreAndCheck(ABytes, ACheck, TGocciaHeapSnapshotLimits.Defaults);
end;

// The refusal's message, or '' when the input restored.
function RestoreRefusal(const ABytes: TBytes;
  const ALimits: TGocciaHeapSnapshotLimits): string;
var
  Engine: TGocciaEngine;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Stream: TBytesStream;
begin
  Result := '';
  Lines := TStringList.Create;
  Engine := nil;
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    try
      Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream, ALimits);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Result := E.Message;
    end;
  finally
    Restore.Free;
    Engine.Free;
    Stream.Free;
    Lines.Free;
  end;
end;

function RestoreRefusal(const ABytes: TBytes): string;
begin
  Result := RestoreRefusal(ABytes, TGocciaHeapSnapshotLimits.Defaults);
end;

function RestoreRefusalIn(const AEngine: TGocciaEngine;
  const ABytes: TBytes): string;
var
  Restore: TGocciaHeapSnapshotRestore;
  Stream: TBytesStream;
begin
  Result := '';
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    try
      Restore := TGocciaHeapSnapshotRestore.Create(AEngine, Stream,
        TGocciaHeapSnapshotLimits.Defaults);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Result := E.Message;
    end;
  finally
    Restore.Free;
    Stream.Free;
  end;
end;

function RoundTrip(const ASource, ACheck: string): string;
var
  Refusal: string;
  Bytes: TBytes;
begin
  Bytes := Snapshot(ASource, Refusal);
  if Refusal <> '' then
    Exit('refused: ' + Refusal);
  Result := RestoreAndCheck(Bytes, ACheck);
end;

function SnapshotModule(const ASource: string;
  out ARefusal: string; const AAsModule: Boolean;
  const AWithEval: Boolean): TBytes;
var
  Engine: TGocciaEngine;
  EvalHost: TGocciaTest262Host;
  Session: TGocciaHeapSnapshotSession;
  Lines: TStringList;
  Capture: TCapture;
  Stream: TBytesStream;
begin
  Result := nil;
  ARefusal := '';
  Lines := TStringList.Create;
  Capture := TCapture.Create;
  Engine := nil;
  EvalHost := nil;
  Session := nil;
  Stream := TBytesStream.Create;
  try
    Lines.Text := ASource;
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    if AAsModule then
      Engine.SourceType := stModule;
    if AWithEval then
      EvalHost := InstallSnapshotEval(Engine);
    Engine.GlobalScope.DefineLexicalBinding('capture',
      TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
    Session :=  TGocciaHeapSnapshotSession.Create(Engine);
    Session.CheckAccounting := False;
    Engine.Execute;
    try
      Session.Snapshot([Capture.Value], Stream);
      Result := Copy(Stream.Bytes, 0, Stream.Size);
    except
      on E: EGocciaHeapSnapshotRefused do
        ARefusal := E.Message;
    end;
  finally
    if Assigned(Capture.Value) then
      TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Stream.Free;
    Session.Free;
    EvalHost.Free;
    Engine.Free;
    Capture.Free;
    Lines.Free;
  end;
end;

function RoundTripModule(const ASource,
  ACheck: string; const AWithEval: Boolean): string;
var
  Refusal: string;
  Bytes: TBytes;
begin
  Bytes := SnapshotModule(ASource, Refusal, False, AWithEval);
  if Refusal <> '' then
    Exit('refused: ' + Refusal);
  Result := RoundTripModuleBytes(ASource, Bytes, ACheck, AWithEval);
end;

function RoundTripModuleBytes(const ASource: string;
  const ABytes: TBytes; const ACheck: string;
  const AWithEval: Boolean): string;
var
  Engine: TGocciaEngine;
  EvalHost: TGocciaTest262Host;
  Restore: TGocciaHeapSnapshotRestore;
  Lines: TStringList;
  Capture: TCapture;
  Stream: TBytesStream;
  Stage: TGocciaGlobalEvalStage;
begin
  Lines := TStringList.Create;
  Capture := TCapture.Create;
  Engine := nil;
  EvalHost := nil;
  Restore := nil;
  Stream := TBytesStream.Create(ABytes);
  try
    Lines.Text := ASource;
    Engine := TGocciaEngine.Create(TEST_PATH, Lines);
    if AWithEval then
      EvalHost := InstallSnapshotEval(Engine);
    Engine.GlobalScope.DefineLexicalBinding('capture',
      TGocciaNativeFunctionValue.Create(Capture.Capture, 'capture', 1), dtConst);
    Engine.Execute;
    try
      Restore := TGocciaHeapSnapshotRestore.Create(Engine, Stream,
        TGocciaHeapSnapshotLimits.Defaults);
    except
      on E: EGocciaHeapSnapshotInvalid do
        Exit('refused: ' + E.Message);
    end;
    TGocciaObjectValue(Engine.Realm.GlobalObject).AssignProperty('restored',
      TGocciaValue(Restore.Roots[0]));
    Result := Engine.EvaluateGlobalEval(ACheck, TEST_PATH,
      Stage).ToStringLiteral.Value;
  finally
    if Assigned(Capture.Value) then
      TGarbageCollector.Instance.RemoveTempRoot(Capture.Value);
    Restore.Free;
    EvalHost.Free;
    Engine.Free;
    Stream.Free;
    Capture.Free;
    Lines.Free;
  end;
end;


function ObjectEntryAt(const ABytes: TBytes;
  const AIndex: Integer): Integer;
var
  Count: Cardinal;
  I: Integer;

  function U32At(const AAt: Integer): Cardinal;
  begin
    Move(ABytes[AAt], Result, 4);
    Result := LEtoN(Result);
  end;

begin
  Result := 4 + 2;
  Result := Result + 4 + 2 * Integer(U32At(Result));  // build identity
  Count := U32At(Result);                               // strings
  Inc(Result, 4);
  for I := 1 to Integer(Count) do
    Result := Result + 4 + 2 * Integer(U32At(Result));
  Result := Result + 4 + 6 * Integer(U32At(Result));  // codecs
  Result := Result + 4 + 4 * Integer(U32At(Result));  // built-in names
  Result := Result + 4 + 8 * Integer(U32At(Result));  // built-ins the run built
  Result := Result + 4 + 4 * Integer(U32At(Result));  // modules
  Expect<Boolean>(AIndex < Integer(U32At(Result))).ToBe(True);
  Inc(Result, 4);
  for I := 1 to AIndex do
    Result := Result + 2 + 4 + Integer(U32At(Result + 2));
end;

end.
