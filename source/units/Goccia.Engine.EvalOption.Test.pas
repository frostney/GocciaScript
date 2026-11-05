program Goccia.Engine.EvalOption.Test;

{$I Goccia.inc}

{ The opt-in eval engine option (EnableEval, #2065). Off by default, so an
  engine and the default runtime have no `eval`. Once enabled, globalThis.eval
  is the realm's %eval%: a call written `eval(...)` is a direct eval (ES2026
  §13.3.6.1) that sees the caller's scope and strictness, and any other call
  is an indirect eval (§19.2.1.1) in the global scope. Each call reports an
  eval.call audit event. }

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Builtins.GlobalEval,
  Goccia.Builtins.GlobalShadowRealm,
  Goccia.CapabilityAudit,
  Goccia.Engine,
  Goccia.Runtime,
  Goccia.TestSetup,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

const
  NON_STRICT_COMPATIBILITY: TGocciaCompatibilityFlags =
    [cfASI, cfVar, cfFunction, cfNonStrictMode];

type
  TEvalSetup = (esEval, esShadowRealm, esFunctionConstructor, esRuntime);
  TEvalSetups = set of TEvalSetup;

  TEvalOptionTests = class(TTestSuite)
  private
    FEvents: TStringList;
    FEventReasons: TStringList;
    FEventLines: TStringList;
    procedure RecordEvent(const AEvent: TGocciaCapabilityAuditEvent);
    function Run(const ASource: string; const ASetups: TEvalSetups;
      const ACompatibility: TGocciaCompatibilityFlags): string;
    function CountEvents(const AEvent, AReason: string;
      out ALines: string): Integer;
    procedure TestAbsentByDefault;
    procedure TestAbsentInDefaultRuntime;
    procedure TestAbsentWithOtherDynamicCodeOptions;
    procedure TestGlobalPropertyShape;
    procedure TestEnableEvalIsIdempotent;
    procedure TestDirectEvalSeesCallerScope;
    procedure TestIndirectEvalUsesGlobalScope;
    procedure TestDirectEvalInheritsStrictness;
    procedure TestIndirectEvalIgnoresCallerStrictness;
    procedure TestDirectEvalInStrictDefaultEngine;
    procedure TestDirectEvalFollowsRealmEvalNotGlobalName;
    procedure TestShadowRealmChildGetsEval;
    procedure TestEvalCallsAreAudited;
  public
    procedure BeforeAll; override;
    procedure AfterAll; override;
    procedure BeforeEach; override;
    procedure SetupTests; override;
  end;

procedure TEvalOptionTests.BeforeAll;
begin
  FEvents := TStringList.Create;
  FEventReasons := TStringList.Create;
  FEventLines := TStringList.Create;
end;

procedure TEvalOptionTests.AfterAll;
begin
  FEvents.Free;
  FEventReasons.Free;
  FEventLines.Free;
end;

procedure TEvalOptionTests.BeforeEach;
begin
  FEvents.Clear;
  FEventReasons.Clear;
  FEventLines.Clear;
end;

procedure TEvalOptionTests.SetupTests;
begin
  Test('eval is absent from an engine by default', TestAbsentByDefault);
  Test('eval is absent from the default runtime',
    TestAbsentInDefaultRuntime);
  Test('Function constructor and ShadowRealm options do not install eval',
    TestAbsentWithOtherDynamicCodeOptions);
  Test('EnableEval installs a writable, configurable, non-constructor eval',
    TestGlobalPropertyShape);
  Test('EnableEval is idempotent', TestEnableEvalIsIdempotent);
  Test('Direct eval sees the caller''s scope', TestDirectEvalSeesCallerScope);
  Test('Indirect eval evaluates in the global scope',
    TestIndirectEvalUsesGlobalScope);
  Test('Direct eval inherits the caller''s strictness',
    TestDirectEvalInheritsStrictness);
  Test('Indirect eval is strict only by its own directive',
    TestIndirectEvalIgnoresCallerStrictness);
  Test('Direct eval works in an engine with default compatibility',
    TestDirectEvalInStrictDefaultEngine);
  Test('Direct eval follows the realm''s %eval%, not the global name',
    TestDirectEvalFollowsRealmEvalNotGlobalName);
  Test('A ShadowRealm child of an eval-enabled realm gets its own eval',
    TestShadowRealmChildGetsEval);
  Test('Direct and indirect eval calls report eval.call audit events',
    TestEvalCallsAreAudited);
end;

procedure TEvalOptionTests.RecordEvent(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  FEvents.Add(CapabilityKindName(AEvent.Kind) + '|' +
    CapabilityDecisionName(AEvent.Decision) + '|' + AEvent.Subject);
  FEventReasons.Add(AEvent.Reason);
  FEventLines.Add(ExtractFileName(AEvent.Source.FilePath) + ':' +
    IntToStr(AEvent.Source.Line));
end;

{ The number of AEvent events with AReason, and the file:line each was
  attributed to, comma-separated, in ALines. }
function TEvalOptionTests.CountEvents(const AEvent, AReason: string;
  out ALines: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  ALines := '';
  for I := 0 to FEvents.Count - 1 do
    if (FEvents[I] = AEvent) and (FEventReasons[I] = AReason) then
    begin
      if Result > 0 then
        ALines := ALines + ',';
      ALines := ALines + FEventLines[I];
      Inc(Result);
    end;
end;

{ Runs ASource as a script and returns String(globalThis.result), or the
  thrown error's message prefixed with "threw: ". }
function TEvalOptionTests.Run(const ASource: string;
  const ASetups: TEvalSetups;
  const ACompatibility: TGocciaCompatibilityFlags): string;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  ResultValue: TGocciaValue;
begin
  Source := TStringList.Create;
  Source.Text := ASource;
  Engine := TGocciaEngine.Create('eval-option.js', Source);
  try
    Engine.Compatibility := ACompatibility;
    Engine.CapabilityAuditSink := RecordEvent;
    if esRuntime in ASetups then
      AttachRuntime(Engine);
    if esFunctionConstructor in ASetups then
      Engine.FunctionConstructor.Enabled := True;
    if esShadowRealm in ASetups then
      EnableShadowRealm(Engine);
    if esEval in ASetups then
    begin
      EnableEval(Engine);
      EnableEval(Engine);
    end;
    try
      Engine.Execute;
      ResultValue := TGocciaObjectValue(Engine.Realm.GlobalObject)
        .GetProperty('result');
      if Assigned(ResultValue) then
        Result := ResultValue.ToStringLiteral.Value
      else
        Result := '<no result>';
    except
      on E: Exception do
        Result := 'threw: ' + E.Message;
    end;
  finally
    Engine.Free;
    Source.Free;
  end;
end;

const
  ABSENCE_PROBE =
    'globalThis.result = [typeof eval, "eval" in globalThis, ' +
    'Object.getOwnPropertyDescriptor(globalThis, "eval") === undefined]' +
    '.join(",");';

procedure TEvalOptionTests.TestAbsentByDefault;
begin
  Expect<string>(Run(ABSENCE_PROBE, [], NON_STRICT_COMPATIBILITY))
    .ToBe('undefined,false,true');
  Expect<string>(Run(ABSENCE_PROBE, [], [])).ToBe('undefined,false,true');
end;

procedure TEvalOptionTests.TestAbsentInDefaultRuntime;
begin
  Expect<string>(Run(ABSENCE_PROBE, [esRuntime], [])).ToBe(
    'undefined,false,true');
end;

procedure TEvalOptionTests.TestAbsentWithOtherDynamicCodeOptions;
begin
  { Neither the realm nor a ShadowRealm child gets eval from the other two
    dynamic-code options. }
  Expect<string>(Run(ABSENCE_PROBE +
    'globalThis.result += "," + new ShadowRealm().evaluate("typeof eval");',
    [esRuntime, esFunctionConstructor, esShadowRealm],
    NON_STRICT_COMPATIBILITY)).ToBe('undefined,false,true,undefined');
end;

procedure TEvalOptionTests.TestGlobalPropertyShape;
begin
  Expect<string>(Run(
    'const d = Object.getOwnPropertyDescriptor(globalThis, "eval");' +
    'let constructs = "no";' +
    'try { new eval(""); constructs = "yes"; }' +
    ' catch (e) { constructs = e instanceof TypeError ? "TypeError" : "other"; }' +
    'globalThis.result = [typeof eval, d.writable, d.enumerable,' +
    ' d.configurable, eval.name, eval.length,' +
    ' Object.hasOwn(eval, "prototype"),' +
    ' Object.getPrototypeOf(eval) === Function.prototype, constructs].join(",");',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe(
    'function,true,false,true,eval,1,false,true,TypeError');
end;

procedure TEvalOptionTests.TestEnableEvalIsIdempotent;
begin
  { Run enables it twice; a second install would replace the function and
    break the identity a direct eval checks. }
  Expect<string>(Run(
    'const first = eval;' +
    'globalThis.result = [first === globalThis.eval,' +
    ' (function () { const x = "local"; return eval("x"); })()].join(",");',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe('true,local');
end;

procedure TEvalOptionTests.TestDirectEvalSeesCallerScope;
begin
  Expect<string>(Run(
    'globalThis.x = "global";' +
    'function f() { const x = "local"; return eval("x"); }' +
    'function g() { var y = 1; eval("y = 2"); return y; }' +
    'function h() { eval("var declared = 3"); return typeof declared; }' +
    'globalThis.result = [f(), g(), h(), typeof declared].join(",");',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe('local,2,number,undefined');
end;

procedure TEvalOptionTests.TestIndirectEvalUsesGlobalScope;
begin
  Expect<string>(Run(
    'globalThis.x = "global";' +
    'const indirect = eval;' +
    'function f() { const x = "local"; return [(0, eval)("x"),' +
    ' indirect("x"), globalThis.eval("x")].join(","); }' +
    'function g() { (0, eval)("var madeGlobal = 4"); return typeof madeGlobal; }' +
    'globalThis.result = [f(), g(), globalThis.madeGlobal,' +
    ' (0, eval)("this") === globalThis, eval(7)].join(",");',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe(
    'global,global,global,number,4,true,7');
end;

procedure TEvalOptionTests.TestDirectEvalInheritsStrictness;
begin
  { §19.2.1.1 step 13: a direct eval in strict code is strict, so its var
    declarations stay in its own environment and `this` in a plain call is
    undefined; in non-strict code they reach the caller. }
  Expect<string>(Run(
    'function strictCaller() { "use strict";' +
    ' eval("var contained = 1");' +
    ' return [typeof contained,' +
    ' eval("(function () { return this; })()") === undefined].join(","); }' +
    'function sloppyCaller() {' +
    ' eval("var leaked = 1");' +
    ' return [typeof leaked,' +
    ' eval("(function () { return this; })()") === globalThis].join(","); }' +
    'globalThis.result = strictCaller() + ";" + sloppyCaller();',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe(
    'undefined,true;number,true');
end;

procedure TEvalOptionTests.TestIndirectEvalIgnoresCallerStrictness;
begin
  Expect<string>(Run(
    'function strictCaller() { "use strict";' +
    ' (0, eval)("var fromStrictCaller = 1");' +
    ' (0, eval)("\"use strict\"; var ownStrict = 1");' +
    ' return [typeof globalThis.fromStrictCaller,' +
    ' typeof globalThis.ownStrict,' +
    ' (0, eval)("(function () { return this; })()") === globalThis].join(","); }' +
    'globalThis.result = strictCaller();',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe('number,undefined,true');
end;

procedure TEvalOptionTests.TestDirectEvalInStrictDefaultEngine;
begin
  { Without cfNonStrictMode every script is strict, so direct eval code is
    strict too and indirect eval is still strict only by its own directive. }
  Expect<string>(Run(
    'const f = () => { const x = "local"; return eval("x"); };' +
    'const g = () => { try { eval("undeclaredName = 1"); return "assigned"; }' +
    ' catch (e) { return e.constructor.name; } };' +
    'globalThis.result = [f(), g(), (0, eval)("1 + 1")].join(",");',
    [esEval], [])).ToBe('local,ReferenceError,2');
end;

procedure TEvalOptionTests.TestDirectEvalFollowsRealmEvalNotGlobalName;
begin
  { §13.3.6.1 step 6.a compares the callee with %eval%, not with whatever
    the global `eval` now holds. }
  Expect<string>(Run(
    'const original = eval;' +
    'function f() { const x = "local"; return eval("x"); }' +
    'globalThis.eval = (s) => "replaced:" + s;' +
    'const afterReplace = f();' +
    'globalThis.eval = original;' +
    'globalThis.result = [afterReplace, f()].join(",");',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe('replaced:x,local');
end;

procedure TEvalOptionTests.TestShadowRealmChildGetsEval;
var
  Lines: string;
begin
  Expect<string>(Run(
    'const realm = new ShadowRealm();' +
    'globalThis.marker = "parent";' +
    'globalThis.result = [realm.evaluate("typeof eval"),' +
    ' realm.evaluate("globalThis.marker = \"child\"; (0, eval)(\"marker\")"),' +
    ' realm.evaluate("(() => { const x = \"local\"; return eval(\"x\"); })()"),' +
    ' marker].join(",");',
    [esEval, esShadowRealm], NON_STRICT_COMPATIBILITY)).ToBe(
    'function,child,local,parent');
  { The child's eval reports to the sink it inherited. }
  Expect<Integer>(CountEvents('eval.call|allow|eval',
    'indirect eval is enabled', Lines)).ToBe(1);
  Expect<Integer>(CountEvents('eval.call|allow|eval',
    'direct eval is enabled', Lines)).ToBe(1);
end;

procedure TEvalOptionTests.TestEvalCallsAreAudited;
var
  Lines: string;
begin
  Expect<string>(Run(
    'function f() { return eval("1"); }' + sLineBreak +
    'f(); f();' + sLineBreak +
    '(0, eval)("2");' + sLineBreak +
    'globalThis.result = "done";',
    [esEval], NON_STRICT_COMPATIBILITY)).ToBe('done');
  { A site that reuses the code it compiled is still a call of eval. Each
    event is attributed to the line of the eval call. }
  Expect<Integer>(CountEvents('eval.call|allow|eval',
    'direct eval is enabled', Lines)).ToBe(2);
  Expect<string>(Lines).ToBe('eval-option.js:1,eval-option.js:1');
  Expect<Integer>(CountEvents('eval.call|allow|eval',
    'indirect eval is enabled', Lines)).ToBe(1);
  Expect<string>(Lines).ToBe('eval-option.js:3');
end;

begin
  TestRunnerProgram.AddSuite(TEvalOptionTests.Create('Engine Eval Option'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
