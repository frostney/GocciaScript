unit Goccia.HeapSnapshot.Exercise;

{ Uses what a heap snapshot restored the way a resumed run might, for the
  programs that feed the reader mutated snapshots (ADR 0148): the hardening
  tests and the fuzz harness. A mutant the reader accepts must be safe to
  use as well as to read, so its roots are walked breadth first through
  their own properties, then their prototypes, up to 600 distinct objects:
  every accessor on the way is read, every function called and every
  generator resumed, returned or thrown into.
  Each step is guarded in the script, so a script error is never the
  outcome; the call runs as a run of its engine (CallAsRun), which waits
  for the jobs the calls left and raises a rejection left unhandled (the
  script handles the promises the functions it calls return, so that is
  rare), a script outcome too. The caller bounds the run with its timeout and
  instruction limit, and only a Pascal exception that is not one of those
  is a finding.

  The script is compiled once per engine: an eval's code stays with its
  engine for the engine's life (ADR 0134), so compiling it for each of
  thousands of mutants would grow the engine and slow every collection. }

{$I Goccia.inc}

interface

uses
  Goccia.Engine,
  Goccia.Values.FunctionBase,
  Goccia.Values.Primitives;

const
  // (restored, mode) => ...: walks at most EXERCISE_OBJECTS distinct
  // objects, what the run's values hold (property values, accessors' results,
  // array elements, what calls and generators return) before any prototype,
  // so the built-ins a prototype chain reaches never crowd out the restored
  // graph. Each generator is resumed (mode 0: next, next, throw, return),
  // returned (mode 1: return, then next) or thrown into (mode 2: throw, then
  // next) while it is still suspended.
  HEAP_SNAPSHOT_EXERCISE = '((restored, mode) => { const seen = new Set(); ' +
    'const own = [restored]; const protos = []; let oi = 0; let pi = 0; ' +
    'let steps = 0; let out = 0; ' +
    'const note = (v) => { if (own.length < 4000) own.push(v); }; ' +
    'const work = { [Symbol.iterator]() { return { next() { ' +
    'if (oi < own.length) return { done: false, value: own[oi++] }; ' +
    'if (pi < protos.length) return { done: false, value: protos[pi++] }; ' +
    'return { done: true, value: undefined }; } }; } }; ' +
    'for (const v of work) { ' +
    'if (v === null || v === undefined) continue; const t = typeof v; ' +
    'if (t !== "object" && t !== "function") { ' +
    'try { out += String(v).length; } catch (e) { out++; } continue; } ' +
    'if (seen.has(v)) continue; seen.add(v); steps++; ' +
    'if (steps > 600) break; ' +
    'try { const p = Object.getPrototypeOf(v); ' +
    'if (protos.length < 4000) protos.push(p); } catch (e) { out++; } ' +
    'try { out += Object.isFrozen(v) ? 1 : 0; ' +
    'out += Object.isExtensible(v) ? 1 : 0; } catch (e) { out++; } ' +
    'let keys = []; try { keys = Reflect.ownKeys(v); } catch (e) { out++; } ' +
    'for (const k of keys) { try { ' +
    'const d = Object.getOwnPropertyDescriptor(v, k); ' +
    'if (d === undefined) continue; if ("value" in d) note(d.value); ' +
    'else { note(d.get); note(d.set); if (d.get) note(d.get.call(v)); } ' +
    '} catch (e) { out++; } } ' +
    'if (Array.isArray(v)) { try { out += v.length > 64 ? 64 : v.length; ' +
    'for (const x of v.slice(0, 64)) note(x); } catch (e) { out++; } } ' +
    'if (t === "function") { try { const r = v(1, "a"); ' +
    'if (r instanceof Promise) r.then(undefined, () => {}); note(r); } ' +
    'catch (e) { out++; } } ' +
    'let tag = ""; try { tag = Object.prototype.toString.call(v); } ' +
    'catch (e) { out++; } ' +
    'if (tag === "[object Generator]") { ' +
    'const uses = mode === 1 ? [() => v.return(7), () => v.next()] : ' +
    'mode === 2 ? [() => v.throw(new Error("x")), () => v.next()] : ' +
    '[() => v.next(5), () => v.next(), () => v.throw(new Error("x")), ' +
    '() => v.return(7)]; ' +
    'for (const step of uses) { try { note(step()); } catch (e) { out++; } } ' +
    '} } return String(steps + out); })';
  // The ways the exercise uses a generator (the script's mode).
  EXERCISE_MODES = 3;

type
  // HEAP_SNAPSHOT_EXERCISE compiled in one engine, held as a GC root until
  // it is freed, which must happen before the engine is.
  THeapSnapshotExerciser = class
  private
    FEngine: TGocciaEngine;
    FFunction: TGocciaFunctionBase;
  public
    constructor Create(const AEngine: TGocciaEngine);
    destructor Destroy; override;
    // Uses ARoot as a resumed run might, generators the way AMode (0 to
    // EXERCISE_MODES - 1) says. Limits and Pascal exceptions propagate.
    procedure Exercise(const ARoot: TGocciaValue; const AMode: Integer);
  end;

// Exercises ARoot once with an exerciser of its own.
procedure ExerciseRestored(const AEngine: TGocciaEngine;
  const ARoot: TGocciaValue; const AMode: Integer);

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.FloatingPoint,
  Goccia.GarbageCollector,
  Goccia.SourcePipeline;

constructor THeapSnapshotExerciser.Create(const AEngine: TGocciaEngine);
var
  Stage: TGocciaGlobalEvalStage;
  FloatingPointState: TGocciaFloatingPointState;
  Value: TGocciaValue;
begin
  inherited Create;
  FEngine := AEngine;
  // EvaluateGlobalEval leaves the engine's floating-point mode to its
  // caller.
  EnterGocciaFloatingPointScope(FloatingPointState);
  try
    Value := AEngine.EvaluateGlobalEval(HEAP_SNAPSHOT_EXERCISE,
      AEngine.SourcePath, Stage);
  finally
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
  FFunction := Value as TGocciaFunctionBase;
  TGarbageCollector.Instance.AddTempRoot(FFunction);
end;

destructor THeapSnapshotExerciser.Destroy;
begin
  if Assigned(FFunction) then
    TGarbageCollector.Instance.RemoveTempRoot(FFunction);
  inherited;
end;

procedure THeapSnapshotExerciser.Exercise(const ARoot: TGocciaValue;
  const AMode: Integer);
var
  Arguments: TGocciaArgumentsCollection;
  Mode: TGocciaValue;
begin
  Mode := TGocciaNumberLiteralValue.Create(AMode mod EXERCISE_MODES);
  TGarbageCollector.Instance.AddTempRoot(Mode);
  Arguments := TGocciaArgumentsCollection.Create([ARoot, Mode]);
  try
    FEngine.CallAsRun(FFunction, Arguments,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Arguments.Free;
    TGarbageCollector.Instance.RemoveTempRoot(Mode);
  end;
end;

procedure ExerciseRestored(const AEngine: TGocciaEngine;
  const ARoot: TGocciaValue; const AMode: Integer);
var
  Exerciser: THeapSnapshotExerciser;
begin
  Exerciser := THeapSnapshotExerciser.Create(AEngine);
  try
    Exerciser.Exercise(ARoot, AMode);
  finally
    Exerciser.Free;
  end;
end;

end.
