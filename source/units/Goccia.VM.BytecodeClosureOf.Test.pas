program Goccia.VM.BytecodeClosureOf.Test;

{ BytecodeClosureOf: the closure behind a bytecode function value, so an
  embedder can read what a script function closed over. }

{$I Goccia.inc}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Engine,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM,
  Goccia.VM.Closure,
  Goccia.VM.Registers;

const
  SCRIPT =
    'const make = () => {' + LineEnding +
    '  let k = 41;' + LineEnding +
    '  let label = "x";' + LineEnding +
    '  return { fn: () => k + label.length, bump: () => { k = 50; } };' +
    LineEnding +
    '};' + LineEnding +
    'const made = make();' + LineEnding +
    'globalThis.fn = made.fn;' + LineEnding +
    'globalThis.bump = made.bump;' + LineEnding +
    'globalThis.fnText = made.fn.toString();' + LineEnding +
    'globalThis.asyncFn = (() => { let n = 7; return async () => n; })();' +
    LineEnding +
    'globalThis.asyncText = globalThis.asyncFn.toString();' + LineEnding +
    'globalThis.native = Math.max;' + LineEnding +
    'globalThis.bound = made.fn.bind(null);' + LineEnding +
    'globalThis.klass = class C { m() { return 1; } };' + LineEnding +
    'globalThis.method = new globalThis.klass().m;' + LineEnding +
    'globalThis.plain = {};' + LineEnding +
    'globalThis.num = 3;' + LineEnding +
    'globalThis.shim = atob;';

  MODULE_SCRIPT =
    'let k = 41;' + LineEnding +
    'export function read() { return k; }' + LineEnding +
    'globalThis.read = read;';

type
  TBytecodeClosureOfTests = class(TTestSuite)
  private
    FSource: TStringList;
    FEngine: TGocciaEngine;
    function Global(const AName: string): TGocciaValue;
    function CapturedValue(const AClosure: TGocciaBytecodeClosure;
      const AName: string): string;
    procedure TestArrowClosure;
    procedure TestCellIsLive;
    procedure TestAsyncArrowClosure;
    procedure TestMethodClosure;
    procedure TestNilForOtherValues;
    procedure TestShimHasNoSourceText;
    procedure TestModuleFunctionResolvesThroughGlobalScope;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

procedure TBytecodeClosureOfTests.SetupTests;
begin
  Test('an arrow function''s closure has its source and captured bindings',
    TestArrowClosure);
  Test('a captured cell shows the binding''s current value', TestCellIsLive);
  Test('an async arrow function has a closure', TestAsyncArrowClosure);
  Test('a class method has a closure', TestMethodClosure);
  Test('any other value has none', TestNilForOtherValues);
  Test('a built-in implemented in bytecode has no source text',
    TestShimHasNoSourceText);
  Test('a module function declaration reads its bindings through GlobalScope',
    TestModuleFunctionResolvesThroughGlobalScope);
end;

procedure TBytecodeClosureOfTests.BeforeAll;
begin
  inherited BeforeAll;
  FSource := TStringList.Create;
  FSource.Text := SCRIPT;
  FEngine := TGocciaEngine.Create('<closure-of>', FSource);
  FEngine.SourceType := stScript;
  FEngine.Execute;
end;

procedure TBytecodeClosureOfTests.AfterAll;
begin
  FEngine.Free;
  FSource.Free;
  inherited AfterAll;
end;

function TBytecodeClosureOfTests.Global(const AName: string): TGocciaValue;
begin
  Result := TGocciaObjectValue(FEngine.Realm.GlobalObject).GetProperty(AName);
end;

{ The captured binding's current value as a string, or '<none>' when the
  closure captured no binding of that name. }
function TBytecodeClosureOfTests.CapturedValue(
  const AClosure: TGocciaBytecodeClosure; const AName: string): string;
var
  I: Integer;
begin
  for I := 0 to AClosure.UpvalueCount - 1 do
    if AClosure.Template.GetUpvalueDescriptor(I).Name = AName then
      Exit(RegisterToValue(AClosure.GetUpvalue(I).Value)
        .ToStringLiteral.Value);
  Result := '<none>';
end;

procedure TBytecodeClosureOfTests.TestArrowClosure;
var
  Closure: TGocciaBytecodeClosure;
begin
  Closure := BytecodeClosureOf(Global('fn'));
  Expect<Boolean>(Assigned(Closure)).ToBe(True);
  Expect<string>(Closure.Template.SourceText)
    .ToBe(Global('fnText').ToStringLiteral.Value);
  Expect<Integer>(Closure.UpvalueCount).ToBe(2);
  Expect<string>(CapturedValue(Closure, 'k')).ToBe('41');
  Expect<string>(CapturedValue(Closure, 'label')).ToBe('x');
  Expect<Boolean>(Assigned(Closure.GlobalScope)).ToBe(True);
end;

procedure TBytecodeClosureOfTests.TestCellIsLive;
var
  Bump: TGocciaValue;
begin
  Bump := Global('bump');
  Expect<Boolean>(Assigned(BytecodeClosureOf(Bump))).ToBe(True);
  TGocciaFunctionBase(Bump).CallNoArgs(
    TGocciaUndefinedLiteralValue.UndefinedValue);
  Expect<string>(CapturedValue(BytecodeClosureOf(Global('fn')), 'k'))
    .ToBe('50');
end;

procedure TBytecodeClosureOfTests.TestAsyncArrowClosure;
var
  Closure: TGocciaBytecodeClosure;
begin
  Closure := BytecodeClosureOf(Global('asyncFn'));
  Expect<Boolean>(Assigned(Closure)).ToBe(True);
  Expect<string>(Closure.Template.SourceText)
    .ToBe(Global('asyncText').ToStringLiteral.Value);
  Expect<Integer>(Closure.UpvalueCount).ToBe(1);
  Expect<string>(CapturedValue(Closure, 'n')).ToBe('7');
end;

procedure TBytecodeClosureOfTests.TestMethodClosure;
begin
  Expect<Boolean>(Assigned(BytecodeClosureOf(Global('method')))).ToBe(True);
end;

procedure TBytecodeClosureOfTests.TestNilForOtherValues;
begin
  Expect<Boolean>(Assigned(BytecodeClosureOf(Global('native')))).ToBe(False);
  Expect<Boolean>(Assigned(BytecodeClosureOf(Global('bound')))).ToBe(False);
  Expect<Boolean>(Assigned(BytecodeClosureOf(Global('klass')))).ToBe(False);
  Expect<Boolean>(Assigned(BytecodeClosureOf(Global('plain')))).ToBe(False);
  Expect<Boolean>(Assigned(BytecodeClosureOf(Global('num')))).ToBe(False);
  Expect<Boolean>(Assigned(BytecodeClosureOf(nil))).ToBe(False);
end;

procedure TBytecodeClosureOfTests.TestShimHasNoSourceText;
var
  Closure: TGocciaBytecodeClosure;
begin
  Closure := BytecodeClosureOf(Global('shim'));
  Expect<Boolean>(Assigned(Closure)).ToBe(True);
  Expect<string>(Closure.Template.SourceText).ToBe('');
end;

{ A function declared at a module's top level reads k by name from its module
  environment: its cell for k stays an undefined placeholder, and k resolves
  through GlobalScope. }
procedure TBytecodeClosureOfTests.TestModuleFunctionResolvesThroughGlobalScope;
var
  Source: TStringList;
  Engine: TGocciaEngine;
  Closure: TGocciaBytecodeClosure;
  Cell: string;
begin
  Source := TStringList.Create;
  Engine := nil;
  try
    Source.Text := MODULE_SCRIPT;
    Engine := TGocciaEngine.Create('<closure-of-module>', Source);
    Engine.SourceType := stModule;
    Engine.Compatibility := [cfFunction];
    Engine.Execute;
    Closure := BytecodeClosureOf(TGocciaObjectValue(Engine.Realm.GlobalObject)
      .GetProperty('read'));
    Expect<Boolean>(Assigned(Closure)).ToBe(True);
    Cell := CapturedValue(Closure, 'k');
    Expect<string>(Cell).ToBe('undefined');
    Expect<string>(Closure.GlobalScope.GetValue('k').ToStringLiteral.Value)
      .ToBe('41');
  finally
    Engine.Free;
    Source.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TBytecodeClosureOfTests.Create(
    'BytecodeClosureOf'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
