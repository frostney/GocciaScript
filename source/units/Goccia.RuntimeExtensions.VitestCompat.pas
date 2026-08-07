unit Goccia.RuntimeExtensions.VitestCompat;

{$I Goccia.inc}

{ Vitest compatibility shim.

  Resolves the bare `vitest` specifier to a small module that re-exports
  `goccia:test` and assembles the `vi` namespace on top of it. The shim ships
  inside the binary as source text rather than as a file on disk, so a suite
  written against Vitest imports the same way under GocciaScript.

  `vi` lives only here. The engine never grows a `vi` namespace, and every
  member the engine cannot honestly provide is a function that throws a named,
  actionable error instead of silently doing nothing.

  Module mocking. `vi.mock(specifier, factory)` is honoured by a pre-pass that
  runs while this extension attaches, i.e. after the engine has stored the
  entry source but before it parses it. The pre-pass parses the entry a second
  time, finds the `vi.mock` / `vi.unmock` calls, resolves each specifier to the
  address the real file would load from, and registers a virtual module at that
  address whose body inlines the factory. Because virtual modules resolve ahead
  of the filesystem, every importer of that address — the test file itself and
  any code under test — links against the mock, and it is the same instance for
  all of them. Nothing in module linking changes.

  Hoisting falls out of this for free: the injection happens before any module
  is parsed, so it does not matter where in the file the `vi.mock` call was
  written. So does lazy factory evaluation: the generated module body runs on
  first import and never if the module is never imported. So does Vitest's rule
  that a factory may not close over outer variables: the factory text is
  relocated into a different module scope, so an outer reference is a genuine
  ReferenceError rather than an emulated one. }

interface

uses
  Goccia.Runtime;

const
  VITEST_COMPAT_SPECIFIER = 'vitest';

type
  TGocciaVitestCompatRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

function VitestCompatShimSource: string;

implementation

uses
  Classes,
  SysUtils,

  TextSemantics,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Engine,
  Goccia.Error,
  Goccia.Modules.Virtual,
  Goccia.SourcePipeline,
  Goccia.Values.Primitives;

const
  { Local binding the generated mock module evaluates the factory into. Prefixed
    so it cannot collide with a plausible export name. }
  MOCK_RESULT_BINDING = '__gocciaMockFactoryResult';
  MOCK_DEFAULT_KEY = 'default';
  MOCK_MEMBER_NAME = 'mock';
  UNMOCK_MEMBER_NAME = 'unmock';
  { The two callee spellings Vitest's own hoisting transform matches. }
  VI_NAMESPACE_NAME = 'vi';
  VITEST_NAMESPACE_NAME = 'vitest';
  DOCS_REFERENCE =
    'See docs/testing-api.md (Vitest compatibility) for the supported surface.';

type
  TGocciaVitestMockDirectiveKind = (vmdkMock, vmdkUnmock);

  TGocciaVitestMockDirective = record
    Kind: TGocciaVitestMockDirectiveKind;
    Address: string;
    Specifier: string;
    ModuleSource: string;
  end;

  { Collects the `vi.mock` / `vi.unmock` directives of one entry file and turns
    the surviving mocks into virtual modules. One instance per engine, i.e. per
    test file, which is also why mock state cannot leak between test files: the
    registry it writes into belongs to that engine's module loader. }
  TGocciaVitestMockHoister = class
  private
    FDirectives: array of TGocciaVitestMockDirective;
    FDirectiveCount: Integer;
    FEngine: TGocciaEngine;

    procedure AddDirective(const AKind: TGocciaVitestMockDirectiveKind;
      const AAddress, ASpecifier, AModuleSource: string);
    function BuildMockModuleSource(const ASpecifier: string;
      const AFactory: TGocciaExpression): string;
    function IsLastDirectiveForAddress(const AIndex: Integer): Boolean;
    procedure CollectCall(const ACall: TGocciaCallExpression;
      const AAtTopLevel: Boolean);
    function TryResolveSpecifier(const ASpecifier: string;
      out AAddress: string): Boolean;
    procedure VisitNode(const ANode: TGocciaASTNode;
      const AAtTopLevel: Boolean);
    procedure WarnNestedDirective(const AMemberName, ASpecifier: string;
      const ACall: TGocciaCallExpression);
  public
    constructor Create(const AEngine: TGocciaEngine);

    procedure Collect(const AProgram: TGocciaProgram);
    procedure InjectModules;
  end;

function EscapeJavaScriptStringLiteral(const AValue: string): string;
var
  Buffer: string;
  I: Integer;
begin
  Buffer := '';
  for I := 1 to Length(AValue) do
    case AValue[I] of
      '\': Buffer := Buffer + '\\';
      '"': Buffer := Buffer + '\"';
      #10: Buffer := Buffer + '\n';
      #13: Buffer := Buffer + '\r';
    else
      Buffer := Buffer + AValue[I];
    end;
  Result := Buffer;
end;

{ Export names are emitted as `export const <name>`, so a key that is not an
  identifier cannot become an export. Deliberately ASCII-only: a non-ASCII
  identifier as a mock export name is not worth a Unicode table here, and the
  factory is reported as unanalysable instead of producing a broken module. }
function IsIdentifierLikeName(const AName: string): Boolean;
var
  I: Integer;
begin
  if AName = '' then
    Exit(False);
  if not (AName[1] in ['A' .. 'Z', 'a' .. 'z', '_', '$']) then
    Exit(False);
  for I := 2 to Length(AName) do
    if not (AName[I] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_', '$']) then
      Exit(False);
  Result := True;
end;

function VitestCompatShimSource: string;
const
  LB = sLineBreak;
begin
  Result :=
    '// GocciaScript vitest compatibility shim. Shipped inside the runner.' + LB +
    'import {' + LB +
    '  describe,' + LB +
    '  test,' + LB +
    '  it,' + LB +
    '  expect,' + LB +
    '  beforeAll,' + LB +
    '  beforeEach,' + LB +
    '  afterEach,' + LB +
    '  afterAll,' + LB +
    '  onTestFinished,' + LB +
    '  mock,' + LB +
    '  spyOn,' + LB +
    '} from "goccia:test";' + LB +
    LB +
    'export {' + LB +
    '  describe,' + LB +
    '  test,' + LB +
    '  it,' + LB +
    '  expect,' + LB +
    '  beforeAll,' + LB +
    '  beforeEach,' + LB +
    '  afterEach,' + LB +
    '  afterAll,' + LB +
    '  onTestFinished,' + LB +
    '  mock,' + LB +
    '  spyOn,' + LB +
    '};' + LB +
    LB +
    'const DOCS =' + LB +
    '  "' + DOCS_REFERENCE + '";' + LB +
    LB +
    'const unsupported = (member, reason) => () => {' + LB +
    '  throw new Error(' + LB +
    '    "vi." + member + " is not supported by the GocciaScript vitest " +' + LB +
    '      "compatibility shim. " + reason + " " + DOCS,' + LB +
    '  );' + LB +
    '};' + LB +
    LB +
    '// vi.mock and vi.unmock are hoisted out of the test file before it is' + LB +
    '// parsed, so by the time the call runs the module graph is already' + LB +
    '// wired: the call itself has nothing left to do.' + LB +
    'const hoistedDirective = (specifier, factory) => undefined;' + LB +
    LB +
    'const DYNAMIC_MOCKING =' + LB +
    '  "vi.mock with a factory is supported and is applied before the module " +' + LB +
    '  "graph is loaded; this member instead has to re-point a module after " +' + LB +
    '  "loading, which needs the module-cache eviction the GocciaScript " +' + LB +
    '  "module loader does not provide.";' + LB +
    'const NO_ACTUAL_MODULE =' + LB +
    '  "vi.mock with a factory is supported, but the real module is not " +' + LB +
    '  "reachable once it is mocked: GocciaScript serves exactly one module " +' + LB +
    '  "per resolved address, so the original cannot be imported alongside " +' + LB +
    '  "the mock. Partial mocks that spread the actual module are therefore " +' + LB +
    '  "not supported either.";' + LB +
    'const FACTORY_SCOPE =' + LB +
    '  "vi.mock with a factory is supported, but the factory is relocated " +' + LB +
    '  "into its own module scope, so there is no shared hoisted-variable " +' + LB +
    '  "scope for it to read.";' + LB +
    'const TYPE_HELPER =' + LB +
    '  "vi.mocked is a TypeScript type helper with no runtime behaviour to " +' + LB +
    '  "provide; call the mock''s own methods instead.";' + LB +
    'const FAKE_TIMERS =' + LB +
    '  "GocciaScript has no fake-timer clock; timers run on the real event loop.";' + LB +
    'const GLOBAL_STUBS =' + LB +
    '  "GocciaScript does not snapshot globals or environment variables, so a " +' + LB +
    '  "stub could not be unwound safely.";' + LB +
    'const MOCK_REGISTRY =' + LB +
    '  "GocciaScript keeps no registry of created mocks; call mockClear, " +' + LB +
    '  "mockReset or mockRestore on the mock itself.";' + LB +
    LB +
    'export const vi = {' + LB +
    '  fn: mock,' + LB +
    '  spyOn: spyOn,' + LB +
    LB +
    '  mock: hoistedDirective,' + LB +
    '  unmock: hoistedDirective,' + LB +
    LB +
    '  doMock: unsupported("doMock", DYNAMIC_MOCKING),' + LB +
    '  doUnmock: unsupported("doUnmock", DYNAMIC_MOCKING),' + LB +
    '  resetModules: unsupported("resetModules", DYNAMIC_MOCKING),' + LB +
    '  mocked: unsupported("mocked", TYPE_HELPER),' + LB +
    '  importActual: unsupported("importActual", NO_ACTUAL_MODULE),' + LB +
    '  importMock: unsupported("importMock", NO_ACTUAL_MODULE),' + LB +
    '  hoisted: unsupported("hoisted", FACTORY_SCOPE),' + LB +
    LB +
    '  useFakeTimers: unsupported("useFakeTimers", FAKE_TIMERS),' + LB +
    '  useRealTimers: unsupported("useRealTimers", FAKE_TIMERS),' + LB +
    '  isFakeTimers: unsupported("isFakeTimers", FAKE_TIMERS),' + LB +
    '  setSystemTime: unsupported("setSystemTime", FAKE_TIMERS),' + LB +
    '  getMockedSystemTime: unsupported("getMockedSystemTime", FAKE_TIMERS),' + LB +
    '  getRealSystemTime: unsupported("getRealSystemTime", FAKE_TIMERS),' + LB +
    '  advanceTimersByTime: unsupported("advanceTimersByTime", FAKE_TIMERS),' + LB +
    '  advanceTimersByTimeAsync:' + LB +
    '    unsupported("advanceTimersByTimeAsync", FAKE_TIMERS),' + LB +
    '  advanceTimersToNextTimer:' + LB +
    '    unsupported("advanceTimersToNextTimer", FAKE_TIMERS),' + LB +
    '  runAllTimers: unsupported("runAllTimers", FAKE_TIMERS),' + LB +
    '  runOnlyPendingTimers: unsupported("runOnlyPendingTimers", FAKE_TIMERS),' + LB +
    LB +
    '  stubEnv: unsupported("stubEnv", GLOBAL_STUBS),' + LB +
    '  stubGlobal: unsupported("stubGlobal", GLOBAL_STUBS),' + LB +
    '  unstubAllEnvs: unsupported("unstubAllEnvs", GLOBAL_STUBS),' + LB +
    '  unstubAllGlobals: unsupported("unstubAllGlobals", GLOBAL_STUBS),' + LB +
    LB +
    '  clearAllMocks: unsupported("clearAllMocks", MOCK_REGISTRY),' + LB +
    '  resetAllMocks: unsupported("resetAllMocks", MOCK_REGISTRY),' + LB +
    '  restoreAllMocks: unsupported("restoreAllMocks", MOCK_REGISTRY),' + LB +
    LB +
    '  waitFor: unsupported("waitFor", FAKE_TIMERS),' + LB +
    '  waitUntil: unsupported("waitUntil", FAKE_TIMERS),' + LB +
    '  setConfig: unsupported("setConfig", GLOBAL_STUBS),' + LB +
    '  resetConfig: unsupported("resetConfig", GLOBAL_STUBS),' + LB +
    '};' + LB +
    LB +
    '// Vitest exports the same namespace object under both names, and hoists' + LB +
    '// `vitest.mock(...)` exactly as it hoists `vi.mock(...)`.' + LB +
    'export const vitest = vi;' + LB;
end;

{ Builds a module whose only job is to fail on first import. Used for the mock
  shapes the shim cannot generate a module for. Reporting at import time rather
  than at hoist time keeps the failure at the point Vitest would have run the
  factory, and keeps a file that never imports the module running. }
function BuildThrowingModuleSource(const ASpecifier, AErrorConstructor,
  AMessage: string): string;
const
  LB = sLineBreak;
begin
  Result :=
    '// GocciaScript vi.mock module for "' +
      EscapeJavaScriptStringLiteral(ASpecifier) + '".' + LB +
    'throw new ' + AErrorConstructor + '("' +
      EscapeJavaScriptStringLiteral(AMessage) + '");' + LB;
end;

{ TGocciaVitestMockHoister }

constructor TGocciaVitestMockHoister.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FDirectiveCount := 0;
end;

procedure TGocciaVitestMockHoister.AddDirective(
  const AKind: TGocciaVitestMockDirectiveKind;
  const AAddress, ASpecifier, AModuleSource: string);
begin
  if FDirectiveCount >= Length(FDirectives) then
    SetLength(FDirectives, Length(FDirectives) * 2 + 4);
  FDirectives[FDirectiveCount].Kind := AKind;
  FDirectives[FDirectiveCount].Address := AAddress;
  FDirectives[FDirectiveCount].Specifier := ASpecifier;
  FDirectives[FDirectiveCount].ModuleSource := AModuleSource;
  Inc(FDirectiveCount);
end;

function TGocciaVitestMockHoister.IsLastDirectiveForAddress(
  const AIndex: Integer): Boolean;
var
  I: Integer;
begin
  for I := AIndex + 1 to FDirectiveCount - 1 do
    if FDirectives[I].Address = FDirectives[AIndex].Address then
      Exit(False);
  Result := True;
end;

function TGocciaVitestMockHoister.BuildMockModuleSource(
  const ASpecifier: string; const AFactory: TGocciaExpression): string;
const
  LB = sLineBreak;
var
  Arrow: TGocciaArrowFunctionExpression;
  EmittedKeys: TStringList;
  Entry: TGocciaPropertySourceOrder;
  ExportsText, FactorySource, Key: string;
  I: Integer;
  ObjectLiteral: TGocciaObjectExpression;
begin
  if not (AFactory is TGocciaArrowFunctionExpression) then
    Exit(BuildThrowingModuleSource(ASpecifier, 'Error',
      '[vitest] vi.mock("' + ASpecifier + '") could not be hoisted by the ' +
      'GocciaScript vitest compatibility shim: the factory must be an arrow ' +
      'function that directly returns an object literal, for example ' +
      'vi.mock("' + ASpecifier + '", () => ({ value: 1 })). ' +
      DOCS_REFERENCE));

  Arrow := TGocciaArrowFunctionExpression(AFactory);
  { An async factory resolves to its object only after a microtask, and the
    generated module has nowhere to await it that its importers could observe. }
  if Arrow.IsAsync then
    Exit(BuildThrowingModuleSource(ASpecifier, 'Error',
      '[vitest] vi.mock("' + ASpecifier + '") could not be hoisted by the ' +
      'GocciaScript vitest compatibility shim: an async factory is not ' +
      'supported, because its exports would only exist after the module has ' +
      'already been linked. Use a synchronous factory. ' + DOCS_REFERENCE));

  if not (Arrow.Body is TGocciaObjectExpression) then
  begin
    if (Arrow.Body is TGocciaLiteralExpression) or
       (Arrow.Body is TGocciaArrayExpression) or
       (Arrow.Body is TGocciaTemplateLiteralExpression) then
      { The factory demonstrably yields a non-object, which is the shape Vitest
        rejects by name — report it the way Vitest does rather than as an
        analysis failure. }
      Exit(BuildThrowingModuleSource(ASpecifier, 'TypeError',
        '[vitest] vi.mock("' + ASpecifier + '", factory?: () => unknown) is ' +
        'not returning an object. Did you mean to return an object with a ' +
        '"default" key?'));

    Exit(BuildThrowingModuleSource(ASpecifier, 'Error',
      '[vitest] vi.mock("' + ASpecifier + '") could not be hoisted by the ' +
      'GocciaScript vitest compatibility shim: only a factory that directly ' +
      'returns an object literal is supported, for example ' +
      'vi.mock("' + ASpecifier + '", () => ({ value: 1 })). ' +
      DOCS_REFERENCE));
  end;

  FactorySource := Copy(AFactory.Span.Source, AFactory.Span.StartOffset + 1,
    AFactory.Span.Length);
  if Trim(FactorySource) = '' then
    Exit(BuildThrowingModuleSource(ASpecifier, 'Error',
      '[vitest] vi.mock("' + ASpecifier + '") could not be hoisted by the ' +
      'GocciaScript vitest compatibility shim: the factory source text could ' +
      'not be recovered. ' + DOCS_REFERENCE));

  ObjectLiteral := TGocciaObjectExpression(Arrow.Body);
  ExportsText := '';
  EmittedKeys := TStringList.Create;
  try
    EmittedKeys.CaseSensitive := True;
    for I := Low(ObjectLiteral.PropertySourceOrder) to
             High(ObjectLiteral.PropertySourceOrder) do
    begin
      Entry := ObjectLiteral.PropertySourceOrder[I];
      { Anything but a plain named property — a spread, a computed key, an
        accessor — hides the export list from static analysis, and guessing it
        would produce a module with silently wrong exports. }
      if Entry.PropertyType <> pstStatic then
        Exit(BuildThrowingModuleSource(ASpecifier, 'Error',
          '[vitest] vi.mock("' + ASpecifier + '") could not be hoisted by ' +
          'the GocciaScript vitest compatibility shim: the factory object ' +
          'literal uses a spread, a computed key or an accessor, so its ' +
          'export names cannot be determined. List the mocked exports as ' +
          'plain named properties instead. ' + DOCS_REFERENCE));

      Key := Entry.StaticKey;
      if not IsIdentifierLikeName(Key) then
        Exit(BuildThrowingModuleSource(ASpecifier, 'Error',
          '[vitest] vi.mock("' + ASpecifier + '") could not be hoisted by ' +
          'the GocciaScript vitest compatibility shim: "' + Key + '" is not ' +
          'usable as an export name. ' + DOCS_REFERENCE));

      if EmittedKeys.IndexOf(Key) >= 0 then
        Continue;
      EmittedKeys.Add(Key);

      if Key = MOCK_DEFAULT_KEY then
        ExportsText := ExportsText + 'export default ' + MOCK_RESULT_BINDING +
          '.' + MOCK_DEFAULT_KEY + ';' + LB
      else
        ExportsText := ExportsText + 'export const ' + Key + ' = ' +
          MOCK_RESULT_BINDING + '.' + Key + ';' + LB;
    end;
  finally
    EmittedKeys.Free;
  end;

  Result :=
    '// GocciaScript vi.mock factory module for "' +
      EscapeJavaScriptStringLiteral(ASpecifier) + '".' + LB +
    'import { vi } from "' + VITEST_COMPAT_SPECIFIER + '";' + LB +
    'const ' + MOCK_RESULT_BINDING + ' = (' + FactorySource + ')();' + LB +
    ExportsText;
end;

function TGocciaVitestMockHoister.TryResolveSpecifier(const ASpecifier: string;
  out AAddress: string): Boolean;
begin
  AAddress := '';
  { Vitest lets a mock name a module that does not exist: nothing fails until
    something actually imports it. Resolution failures are therefore dropped
    here so the real import can fail on its own terms later. }
  try
    AAddress := FEngine.ModuleLoader.ResolveModuleAddress(ASpecifier,
      FEngine.SourcePath);
  except
    on Exception do
      Exit(False);
  end;
  Result := AAddress <> '';
end;

procedure TGocciaVitestMockHoister.WarnNestedDirective(const AMemberName,
  ASpecifier: string; const ACall: TGocciaCallExpression);
begin
  { Written to ErrOutput, like the loader's virtual-module shadow warning: the
    runner's JSON envelope goes to stdout, and this warning is produced before
    the runner has had a chance to set SuppressWarnings on the engine. }
  WriteLn(ErrOutput, Format(
    'Warning: A vi.%s("%s") call in "%s" is not at the top level of the ' +
    'module. Although it appears nested, it will be hoisted and executed ' +
    'before any tests run. Move it to the top level to reflect its actual ' +
    'execution order. This will become an error in a future version.',
    [AMemberName, ASpecifier, FEngine.SourcePath]));
  WriteLn(ErrOutput, Format('  --> %s:%d:%d',
    [FEngine.SourcePath, ACall.Span.StartLine, ACall.Span.StartColumn]));
end;

procedure TGocciaVitestMockHoister.CollectCall(
  const ACall: TGocciaCallExpression; const AAtTopLevel: Boolean);
var
  Address, MemberName, NamespaceName, Specifier: string;
  Callee: TGocciaMemberExpression;
  Literal: TGocciaLiteralExpression;
begin
  // Vitest's hoist is a syntactic transform over the literal `vi.mock` and
  // `vitest.mock` shapes. An aliased or namespaced callee — `v.mock(...)`
  // after importing `vi as v`, or `ns.vi.mock(...)` after a namespace import —
  // is deliberately NOT hoisted by Vitest, and the mock then silently never
  // applies. Matching only the literal shapes, with the runtime member a
  // no-op, reproduces that exactly; the silence is parity, not an oversight.
  if not (ACall.Callee is TGocciaMemberExpression) then
    Exit;
  Callee := TGocciaMemberExpression(ACall.Callee);
  if Callee.Computed or not (Callee.ObjectExpr is TGocciaIdentifierExpression) then
    Exit;
  NamespaceName := TGocciaIdentifierExpression(Callee.ObjectExpr).Name;
  if (NamespaceName <> VI_NAMESPACE_NAME) and
     (NamespaceName <> VITEST_NAMESPACE_NAME) then
    Exit;

  MemberName := Callee.PropertyName;
  if (MemberName <> MOCK_MEMBER_NAME) and (MemberName <> UNMOCK_MEMBER_NAME) then
    Exit;

  { Vitest requires a literal path so the transform can rewrite it. Anything
    else is left alone, and the runtime member is a no-op. }
  if ACall.Arguments.Count < 1 then
    Exit;
  if not (ACall.Arguments[0] is TGocciaLiteralExpression) then
    Exit;
  Literal := TGocciaLiteralExpression(ACall.Arguments[0]);
  if not (Literal.Value is TGocciaStringLiteralValue) then
    Exit;
  Specifier := TGocciaStringLiteralValue(Literal.Value).Value;

  if not TryResolveSpecifier(Specifier, Address) then
    Exit;

  if not AAtTopLevel then
    WarnNestedDirective(MemberName, Specifier, ACall);

  if MemberName = UNMOCK_MEMBER_NAME then
  begin
    AddDirective(vmdkUnmock, Address, Specifier, '');
    Exit;
  end;

  if ACall.Arguments.Count < 2 then
  begin
    AddDirective(vmdkMock, Address, Specifier,
      BuildThrowingModuleSource(Specifier, 'Error',
        '[vitest] vi.mock("' + Specifier + '") without a factory is not ' +
        'implemented by the GocciaScript vitest compatibility shim: ' +
        'automocking would have to deep-wrap the real module''s exports. ' +
        'Pass a factory instead, for example vi.mock("' + Specifier +
        '", () => ({ value: 1 })). ' + DOCS_REFERENCE));
    Exit;
  end;

  AddDirective(vmdkMock, Address, Specifier,
    BuildMockModuleSource(Specifier, ACall.Arguments[1]));
end;

procedure TGocciaVitestMockHoister.VisitNode(const ANode: TGocciaASTNode;
  const AAtTopLevel: Boolean);
var
  BlockStatement: TGocciaBlockStatement;
  CaseClause: TGocciaCaseClause;
  CallExpression: TGocciaCallExpression;
  I, J: Integer;
  SwitchStatement: TGocciaSwitchStatement;
  TryStatement: TGocciaTryStatement;
  VariableDeclaration: TGocciaVariableDeclaration;
begin
  if not Assigned(ANode) then
    Exit;

  if ANode is TGocciaExpressionStatement then
    { An expression statement is transparent for the purpose of "is this at the
      top level of the module": `vi.mock(...)` written as a bare statement in
      the program body is the non-nested form. }
    VisitNode(TGocciaExpressionStatement(ANode).Expression, AAtTopLevel)
  else if ANode is TGocciaBlockStatement then
  begin
    BlockStatement := TGocciaBlockStatement(ANode);
    for I := 0 to BlockStatement.Nodes.Count - 1 do
      VisitNode(BlockStatement.Nodes[I], False);
  end
  else if ANode is TGocciaIfStatement then
  begin
    VisitNode(TGocciaIfStatement(ANode).Consequent, False);
    VisitNode(TGocciaIfStatement(ANode).Alternate, False);
  end
  else if ANode is TGocciaForStatement then
  begin
    VisitNode(TGocciaForStatement(ANode).Init, False);
    VisitNode(TGocciaForStatement(ANode).Body, False);
  end
  else if ANode is TGocciaWhileStatement then
    VisitNode(TGocciaWhileStatement(ANode).Body, False)
  else if ANode is TGocciaDoWhileStatement then
    VisitNode(TGocciaDoWhileStatement(ANode).Body, False)
  else if ANode is TGocciaForOfStatement then
    VisitNode(TGocciaForOfStatement(ANode).Body, False)
  else if ANode is TGocciaForInStatement then
    VisitNode(TGocciaForInStatement(ANode).Body, False)
  else if ANode is TGocciaTryStatement then
  begin
    TryStatement := TGocciaTryStatement(ANode);
    VisitNode(TryStatement.Block, False);
    VisitNode(TryStatement.CatchBlock, False);
    VisitNode(TryStatement.FinallyBlock, False);
  end
  else if ANode is TGocciaSwitchStatement then
  begin
    SwitchStatement := TGocciaSwitchStatement(ANode);
    for I := 0 to SwitchStatement.Cases.Count - 1 do
    begin
      CaseClause := SwitchStatement.Cases[I];
      for J := 0 to CaseClause.Consequent.Count - 1 do
        VisitNode(CaseClause.Consequent[J], False);
    end;
  end
  else if ANode is TGocciaReturnStatement then
    VisitNode(TGocciaReturnStatement(ANode).Value, False)
  else if ANode is TGocciaVariableDeclaration then
  begin
    VariableDeclaration := TGocciaVariableDeclaration(ANode);
    for I := Low(VariableDeclaration.Variables) to
             High(VariableDeclaration.Variables) do
      VisitNode(VariableDeclaration.Variables[I].Initializer, False);
  end
  else if ANode is TGocciaDestructuringDeclaration then
    VisitNode(TGocciaDestructuringDeclaration(ANode).Initializer, False)
  else if ANode is TGocciaFunctionDeclaration then
    VisitNode(TGocciaFunctionDeclaration(ANode).FunctionExpression, False)
  else if ANode is TGocciaExportVariableDeclaration then
    VisitNode(TGocciaExportVariableDeclaration(ANode).Declaration, AAtTopLevel)
  else if ANode is TGocciaExportFunctionDeclaration then
    VisitNode(TGocciaExportFunctionDeclaration(ANode).Declaration, False)
  else if ANode is TGocciaCallExpression then
  begin
    CallExpression := TGocciaCallExpression(ANode);
    CollectCall(CallExpression, AAtTopLevel);
    // A callback argument is where a nested directive realistically hides, as
    // in a `vi.mock` written inside a `describe` or `test` callback.
    for I := 0 to CallExpression.Arguments.Count - 1 do
      VisitNode(CallExpression.Arguments[I], False);
  end
  else if ANode is TGocciaArrowFunctionExpression then
    VisitNode(TGocciaArrowFunctionExpression(ANode).Body, False)
  else if ANode is TGocciaFunctionExpression then
    VisitNode(TGocciaFunctionExpression(ANode).Body, False);
end;

procedure TGocciaVitestMockHoister.Collect(const AProgram: TGocciaProgram);
var
  I: Integer;
begin
  if not Assigned(AProgram) then
    Exit;
  for I := 0 to AProgram.Body.Count - 1 do
    VisitNode(AProgram.Body[I], True);
end;

procedure TGocciaVitestMockHoister.InjectModules;
var
  I: Integer;
begin
  { Mock and unmock apply in source order, last one wins — so an address whose
    final directive is an unmock keeps its real file, and a repeated mock keeps
    only the last factory. }
  for I := 0 to FDirectiveCount - 1 do
  begin
    if FDirectives[I].Kind = vmdkUnmock then
      Continue;
    if not IsLastDirectiveForAddress(I) then
      Continue;
    FEngine.ModuleLoader.InjectModule(FDirectives[I].Address,
      FDirectives[I].ModuleSource, 'javascript', '',
      VIRTUAL_MODULE_PROVENANCE_VITEST_MOCK);
  end;
end;

{ Finds the `vi.mock` / `vi.unmock` calls in the entry source and turns them
  into virtual modules before the engine parses that source for real. }
procedure HoistModuleMocks(const AEngine: TGocciaEngine);
var
  Hoister: TGocciaVitestMockHoister;
  PipelineOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  SourceText: string;
begin
  if not Assigned(AEngine.SourceLines) then
    Exit;
  SourceText := StringListToSourceText(AEngine.SourceLines);
  { The overwhelming majority of files mention none of these, and a substring
    scan costs far less than the parse below. }
  if (Pos('vi.mock', SourceText) = 0) and (Pos('vi.unmock', SourceText) = 0) and
     (Pos('vitest.mock', SourceText) = 0) and
     (Pos('vitest.unmock', SourceText) = 0) then
    Exit;

  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := AEngine.Preprocessors;
  { Every compatibility flag on, deliberately. The CLI applies the file's real
    flags after extensions attach, so they cannot be read here — and this parse
    only has to locate `vi.mock` call shapes, never to judge the program. A
    maximally permissive parse therefore succeeds whenever the engine's own
    parse would, which matters because the alternative is a file that quietly
    fails to hoist. The engine's parse stays the one that reports the errors. }
  PipelineOptions.Compatibility :=
    [Low(TGocciaCompatibility) .. High(TGocciaCompatibility)];
  PipelineOptions.LabelStatementsEnabled := True;
  PipelineOptions.ForInLoopsEnabled := True;
  PipelineOptions.ExperimentalJSModuleSourceEnabled := True;

  { A second parse of one file, measured at tens of microseconds, bought only
    by files that actually mention vi.mock. That is the price of hoisting
    without touching module linking, and it is worth paying.

    Module first, then script: module source accepts `import.meta` and the
    script source accepts sloppy-mode constructs, and neither is a superset of
    the other. Trying both means a directory that opts into `source-type:
    module` through a config file the engine has not applied yet still hoists. }
  PipelineResult := nil;
  try
    PipelineOptions.SourceType := stModule;
    try
      PipelineResult := TGocciaSourcePipeline.Parse(AEngine.SourceLines,
        AEngine.SourcePath, PipelineOptions);
    except
      on TGocciaError do
        PipelineResult := nil;
    end;

    if not Assigned(PipelineResult) then
    begin
      PipelineOptions.SourceType := stScript;
      try
        PipelineResult := TGocciaSourcePipeline.Parse(AEngine.SourceLines,
          AEngine.SourcePath, PipelineOptions);
      except
        on TGocciaError do
          Exit;
      end;
    end;

    Hoister := TGocciaVitestMockHoister.Create(AEngine);
    try
      Hoister.Collect(PipelineResult.ProgramNode);
      Hoister.InjectModules;
    finally
      Hoister.Free;
    end;
  finally
    PipelineResult.Free;
  end;
end;

procedure TGocciaVitestCompatRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  Runtime.Engine.ModuleLoader.InjectModule(VITEST_COMPAT_SPECIFIER,
    VitestCompatShimSource);
  HoistModuleMocks(Runtime.Engine);
end;

end.
