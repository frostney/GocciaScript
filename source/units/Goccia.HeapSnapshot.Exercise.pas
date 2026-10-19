unit Goccia.HeapSnapshot.Exercise;

{ Uses what a heap snapshot restored the way a resumed run might, for the
  programs that feed the reader mutated snapshots (ADR 0148): the hardening
  tests and the fuzz harness. A mutant the reader accepts must be safe to
  use as well as to read, so its roots are walked breadth first through
  their own properties and prototypes, every accessor is read, every
  function called and every generator resumed, returned and thrown into.
  Each step is guarded in the script, so a script error is never the
  outcome; the caller bounds the run with its timeout and instruction limit,
  and only a Pascal exception that is not one of those is a finding. }

{$I Goccia.inc}

interface

uses
  Goccia.Engine,
  Goccia.Values.Primitives;

const
  HEAP_SNAPSHOT_EXERCISE = '(() => { const seen = new Set(); ' +
    'const queue = [restored]; let steps = 0; let out = 0; ' +
    'const note = (v) => { if (queue.length < 4000) queue.push(v); }; ' +
    'for (const v of queue) { steps++; if (steps > 600) break; ' +
    'if (v === null || v === undefined) continue; const t = typeof v; ' +
    'if (t !== "object" && t !== "function") { ' +
    'try { out += String(v).length; } catch (e) { out++; } continue; } ' +
    'if (seen.has(v)) continue; seen.add(v); ' +
    'try { note(Object.getPrototypeOf(v)); } catch (e) { out++; } ' +
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
    'if (t === "function") { try { note(v(1, "a")); } catch (e) { out++; } } ' +
    'let tag = ""; try { tag = Object.prototype.toString.call(v); } ' +
    'catch (e) { out++; } ' +
    'if (tag === "[object Generator]") { for (const step of [' +
    '() => v.next(5), () => v.next(), () => v.throw(new Error("x")), ' +
    '() => v.return(7)]) { try { note(step()); } catch (e) { out++; } } } } ' +
    'return String(steps + out); })()';

// Runs HEAP_SNAPSHOT_EXERCISE in AEngine with ARoot as `restored`, a global
// it removes again. Limits and Pascal exceptions propagate.
procedure ExerciseRestored(const AEngine: TGocciaEngine;
  const ARoot: TGocciaValue);

implementation

uses
  Goccia.FloatingPoint,
  Goccia.SourcePipeline,
  Goccia.Values.ObjectValue;

procedure ExerciseRestored(const AEngine: TGocciaEngine;
  const ARoot: TGocciaValue);
var
  Global: TGocciaObjectValue;
  Stage: TGocciaGlobalEvalStage;
  FloatingPointState: TGocciaFloatingPointState;
begin
  // The script's arithmetic needs the engine's floating-point mode, which
  // EvaluateGlobalEval leaves to its caller.
  EnterGocciaFloatingPointScope(FloatingPointState);
  Global := TGocciaObjectValue(AEngine.Realm.GlobalObject);
  Global.AssignProperty('restored', ARoot);
  try
    AEngine.EvaluateGlobalEval(HEAP_SNAPSHOT_EXERCISE, AEngine.SourcePath,
      Stage);
  finally
    Global.DeleteProperty('restored');
    LeaveGocciaFloatingPointScope(FloatingPointState);
  end;
end;

end.
