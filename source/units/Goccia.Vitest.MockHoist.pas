unit Goccia.Vitest.MockHoist;

{$I Goccia.inc}

{ vi.mock hoisting for the vitest compatibility shim, free of any engine.

  A pre-pass over a test file's source finds its `vi.mock` / `vi.unmock`
  calls, resolves each specifier to the address the real file would load from
  (through the caller's resolver), and builds for each surviving mock a module
  source that inlines the factory. The caller registers each one at its address
  ahead of the filesystem, so every importer of that address links against the
  same mock. See Goccia.RuntimeExtensions.VitestCompat and docs/testing-api.md
  (Vitest compatibility). }

interface

uses
  Classes,

  Goccia.SourcePipeline;

type
  { Resolves ASpecifier as an import in AReferrer would; False (or an
    exception) when it does not resolve. }
  TGocciaMockResolveFunc = function(const ASpecifier, AReferrer: string;
    out AAddress: string): Boolean of object;

  TGocciaMockModule = record
    Address: string;
    Source: string;
  end;
  TGocciaMockModules = array of TGocciaMockModule;

{ The mock modules a test file's `vi.mock` calls declare, in source order, the
  last directive for an address winning; none when the source mentions no
  directive or does not parse. }
function CollectModuleMocks(const ASourceLines: TStringList;
  const ASourcePath: string; const APreprocessors: TGocciaPreprocessors;
  const AResolve: TGocciaMockResolveFunc): TGocciaMockModules;

implementation

uses
  Generics.Collections,
  SysUtils,

  TextSemantics,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.EngineFault,
  Goccia.Keywords.Reserved;

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
    the surviving mocks into module sources. One instance per test file, which
    is also why mock state cannot leak between test files. }
  TGocciaVitestMockHoister = class
  private
    FDirectives: array of TGocciaVitestMockDirective;
    FDirectiveCount: Integer;
    FSourcePath: string;
    FResolve: TGocciaMockResolveFunc;

    procedure AddDirective(const AKind: TGocciaVitestMockDirectiveKind;
      const AAddress, ASpecifier, AModuleSource: string);
    function BuildMockModuleSource(const ASpecifier: string;
      const AFactory: TGocciaExpression): string;
    function IsLastDirectiveForAddress(const AIndex: Integer): Boolean;
    procedure CollectCall(const ACall: TGocciaCallExpression;
      const AAtTopLevel: Boolean);
    function TryResolveSpecifier(const ASpecifier: string;
      out AAddress: string): Boolean;
    procedure VisitClassDefinition(const ADefinition: TGocciaClassDefinition);
    procedure VisitExpressions(const AExpressions: TObjectList<TGocciaExpression>);
    procedure VisitNode(const ANode: TGocciaASTNode;
      const AAtTopLevel: Boolean);
    procedure WarnNestedDirective(const AMemberName, ASpecifier: string;
      const ACall: TGocciaCallExpression);
  public
    constructor Create(const ASourcePath: string; const AResolve: TGocciaMockResolveFunc);

    procedure Collect(const AProgram: TGocciaProgram);
    function MockModules: TGocciaMockModules;
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

{ Whether a name may appear as the binding in `export const <name>`.

  Reserved words and strict-mode reserved words are not BindingIdentifiers. Nor
  are `await` (reserved outright in a module goal), `yield`, `eval` or
  `arguments` (barred as strict-mode BindingIdentifiers) — module source is
  always strict, and no keyword predicate lists those four. Node agrees:
  `export const await = 1;` and each of the other three are rejected under
  `--input-type=module`.

  Anything this returns False for is still exportable, just through an export
  clause with an alias rather than a `const` declaration. }
function IsBindableName(const AName: string): Boolean;
begin
  Result := not (IsReservedKeyword(AName) or IsStrictModeReservedKeyword(AName)
    or (AName = 'await') or (AName = 'yield') or (AName = 'eval')
    or (AName = 'arguments'));
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

{ Whether any factory key starts with APrefix followed only by digits, i.e.
  could name one of the alias locals this prefix will generate. }
function AliasPrefixCollides(const AKeys: TStringList;
  const APrefix: string): Boolean;
var
  I, J: Integer;
  Tail: string;
  AllDigits: Boolean;
begin
  for I := 0 to AKeys.Count - 1 do
  begin
    if Copy(AKeys[I], 1, Length(APrefix)) <> APrefix then
      Continue;
    Tail := Copy(AKeys[I], Length(APrefix) + 1, MaxInt);
    if Tail = '' then
      Exit(True);
    AllDigits := True;
    for J := 1 to Length(Tail) do
      if not (Tail[J] in ['0' .. '9']) then
      begin
        AllDigits := False;
        Break;
      end;
    if AllDigits then
      Exit(True);
  end;
  Result := False;
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

constructor TGocciaVitestMockHoister.Create(const ASourcePath: string;
  const AResolve: TGocciaMockResolveFunc);
begin
  inherited Create;
  FSourcePath := ASourcePath;
  FResolve := AResolve;
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
  AliasCount: Integer;
  AliasLocal, AliasPrefix, ResultBinding: string;
  Arrow: TGocciaArrowFunctionExpression;
  EmittedKeys: TStringList;
  Entry: TGocciaPropertySourceOrder;
  ExportsText, FactorySource, Key: string;
  I: Integer;
  ObjectLiteral: TGocciaObjectExpression;
begin
  AliasCount := 0;
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
    { The parser produces TGocciaTemplateLiteralExpression only for a template
      with no substitutions; an interpolated one is a
      TGocciaTemplateWithInterpolationExpression. Both demonstrably yield a
      string, so both belong here rather than in the generic analysis-failure
      branch below. }
    if (Arrow.Body is TGocciaLiteralExpression) or
       (Arrow.Body is TGocciaArrayExpression) or
       (Arrow.Body is TGocciaTemplateLiteralExpression) or
       (Arrow.Body is TGocciaTemplateWithInterpolationExpression) then
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

    { Pick binding names the factory's own keys cannot collide with.

      The generated module declares a local for the factory result, and one
      per aliased export. Both used fixed names, so a factory exporting
      `__gocciaMockFactoryResult` produced
      `const X = …; export const X = X.X;` — a redeclaration that fails to
      parse, reproduced as "Identifier '__gocciaMockFactoryResult' has already
      been declared". The alias locals carry the same hazard one layer deeper,
      since a factory may name a key `__gocciaMockFactoryResultAlias1`.

      Every static key is collected first and each generated name grows an
      underscore until it is outside that set, so the names are chosen against
      the actual factory rather than hoped to be improbable. }
    for I := Low(ObjectLiteral.PropertySourceOrder) to
             High(ObjectLiteral.PropertySourceOrder) do
      if ObjectLiteral.PropertySourceOrder[I].PropertyType = pstStatic then
        EmittedKeys.Add(ObjectLiteral.PropertySourceOrder[I].StaticKey);

    ResultBinding := MOCK_RESULT_BINDING;
    while EmittedKeys.IndexOf(ResultBinding) >= 0 do
      ResultBinding := ResultBinding + '_';

    AliasPrefix := ResultBinding + 'Alias';
    while AliasPrefixCollides(EmittedKeys, AliasPrefix) do
      AliasPrefix := AliasPrefix + '_';

    EmittedKeys.Clear;
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
        ExportsText := ExportsText + 'export default ' + ResultBinding +
          '.' + MOCK_DEFAULT_KEY + ';' + LB
      else if IsBindableName(Key) then
        ExportsText := ExportsText + 'export const ' + Key + ' = ' +
          ResultBinding + '.' + Key + ';' + LB
      else
      begin
        { A key that is identifier-like but not a BindingIdentifier — a
          reserved word like `class`, or one of await/yield/eval/arguments,
          which module code cannot bind. `export const <name>` would be a
          syntax error, but an export CLAUSE takes any IdentifierName as the
          exported name, so the value binds to a generated local and is
          exported under an alias.

          Rejecting these instead, as this code did, was measured against the
          pinned oracle and found wrong in both directions: Vitest 4.1.10
          exposes every one of them as a module export (probed with a factory
          returning await/yield/eval/arguments and again with
          class/static/import/function — all readable through the namespace),
          so refusing the mock diverges from the product target just as surely
          as emitting an invalid `export const` did. An export clause aliasing
          a local to the name `await` is accepted by Node and parses under
          GocciaScript too, so the alias form is the one shape valid
          everywhere. Property access is safe
          unaliased: any IdentifierName may follow a dot. }
        Inc(AliasCount);
        AliasLocal := AliasPrefix + IntToStr(AliasCount);
        ExportsText := ExportsText +
          'const ' + AliasLocal + ' = ' + ResultBinding + '.' + Key +
            ';' + LB +
          'export { ' + AliasLocal + ' as ' + Key + ' };' + LB;
      end;
    end;
  finally
    EmittedKeys.Free;
  end;

  Result :=
    '// GocciaScript vi.mock factory module for "' +
      EscapeJavaScriptStringLiteral(ASpecifier) + '".' + LB +
    'import { vi } from "vitest";' + LB +
    'const ' + ResultBinding + ' = (' + FactorySource + ')();' + LB +
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
    if not FResolve(ASpecifier, FSourcePath, AAddress) then
      Exit(False);
  except
    on E: Exception do
    begin
      if IsEngineIntegrityFault(E) then
        raise;
      Exit(False);
    end;
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
    [AMemberName, ASpecifier, FSourcePath]));
  WriteLn(ErrOutput, Format('  --> %s:%d:%d',
    [FSourcePath, ACall.Span.StartLine, ACall.Span.StartColumn]));
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
  if Literal.Kind <> lkString then
    Exit;
  Specifier := Literal.StringValue;

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

procedure TGocciaVitestMockHoister.VisitExpressions(
  const AExpressions: TObjectList<TGocciaExpression>);
var
  I: Integer;
begin
  if not Assigned(AExpressions) then
    Exit;
  for I := 0 to AExpressions.Count - 1 do
    VisitNode(AExpressions[I], False);
end;

procedure TGocciaVitestMockHoister.VisitClassDefinition(
  const ADefinition: TGocciaClassDefinition);
var
  Getter: TGocciaGetterExpression;
  I: Integer;
  Initializer: TGocciaExpression;
  Method: TGocciaClassMethod;
  Setter: TGocciaSetterExpression;

  procedure VisitMethods(const AMethods: TGocciaClassMethodMap);
  begin
    if not Assigned(AMethods) then
      Exit;
    for Method in AMethods.Values do
      if Assigned(Method) then
        VisitNode(Method.Body, False);
  end;

  procedure VisitProperties(const AProperties: TGocciaExpressionMap);
  begin
    if not Assigned(AProperties) then
      Exit;
    for Initializer in AProperties.Values do
      VisitNode(Initializer, False);
  end;

  procedure VisitGetters(const AGetters: TGocciaGetterExpressionMap);
  begin
    if not Assigned(AGetters) then
      Exit;
    for Getter in AGetters.Values do
      if Assigned(Getter) then
        VisitNode(Getter.Body, False);
  end;

  procedure VisitSetters(const ASetters: TGocciaSetterExpressionMap);
  begin
    if not Assigned(ASetters) then
      Exit;
    for Setter in ASetters.Values do
      if Assigned(Setter) then
        VisitNode(Setter.Body, False);
  end;

begin
  if not Assigned(ADefinition) then
    Exit;

  VisitNode(ADefinition.SuperClassExpression, False);
  VisitMethods(ADefinition.Methods);
  VisitMethods(ADefinition.StaticMethods);
  VisitMethods(ADefinition.PrivateMethods);
  VisitProperties(ADefinition.InstanceProperties);
  VisitProperties(ADefinition.StaticProperties);
  VisitProperties(ADefinition.PrivateInstanceProperties);
  VisitProperties(ADefinition.PrivateStaticProperties);
  VisitGetters(ADefinition.Getters);
  VisitGetters(ADefinition.StaticGetters);
  VisitSetters(ADefinition.Setters);
  VisitSetters(ADefinition.StaticSetters);

  { FElements records every class element, not only static blocks and
    `accessor` fields as an earlier comment here claimed — the parser appends
    cekMethod, cekGetter, cekSetter, cekField and cekAccessor to it as well.
    That wrong description is what kept this loop looking at static blocks
    alone.

    The maps above are keyed by an element's static name, so they reach every
    NAMED element's body and nothing else. Two things they structurally cannot
    reach, because a computed element has no name to be keyed by:

      - the key expression itself, and
      - the body or initializer of a computed element.

    Both are visited here, and both were silently dropped before: a vi.mock
    written inside a computed key, or inside the body of a method whose key is
    computed, never registered and nothing said so. Named elements are NOT
    revisited from here — the maps already covered them, and a second visit
    would print the nested-directive warning twice. }
  for I := Low(ADefinition.FElements) to High(ADefinition.FElements) do
  begin
    if ADefinition.FElements[I].Kind = cekStaticBlock then
    begin
      VisitNode(ADefinition.FElements[I].StaticBlockBody, False);
      Continue;
    end;

    { An instance auto-accessor's initializer is in PrivateInstanceProperties
      under its storage name, so only a static one is visited here. Its
      getter and setter are synthesized and hold no directive. }
    if ADefinition.FElements[I].Kind = cekAccessor then
    begin
      VisitNode(ADefinition.FElements[I].ComputedKeyExpression, False);
      if ADefinition.FElements[I].IsStatic then
        VisitNode(ADefinition.FElements[I].FieldInitializer, False);
      Continue;
    end;

    if not ADefinition.FElements[I].IsComputed then
      Continue;

    VisitNode(ADefinition.FElements[I].ComputedKeyExpression, False);

    if Assigned(ADefinition.FElements[I].MethodNode) then
      VisitNode(ADefinition.FElements[I].MethodNode.Body, False);
    if Assigned(ADefinition.FElements[I].GetterNode) then
      VisitNode(ADefinition.FElements[I].GetterNode.Body, False);
    if Assigned(ADefinition.FElements[I].SetterNode) then
      VisitNode(ADefinition.FElements[I].SetterNode.Body, False);
    VisitNode(ADefinition.FElements[I].FieldInitializer, False);
  end;
end;

{ A structural walk over every child position an expression or a statement can
  hold, so that a `vi.mock` is found wherever it is written.

  The walk is deliberately exhaustive rather than "the places a directive
  plausibly appears". Vitest's own hoisting transform walks the whole AST
  (`esmWalker` + `onCallExpression` in @vitest/mocker), so anything this walk
  fails to reach is a directive Vitest would have hoisted and GocciaScript
  silently drops — the worst possible failure mode, because the mock simply
  never applies and nothing says so. Function bodies in particular hide behind
  ordinary expression nodes: an arrow holding a directive can sit inside a call
  *callee* (everything but the trailing member call of
  `expect(f(cb)).toBe(x)`), inside a conditional, inside an array element,
  inside an object property, or inside a class method — none of which the
  previous statement-and-arguments-only walk reached.

  What does NOT change here is which callees count: only the literal `vi.mock`
  / `vitest.mock` spellings are collected, at any depth. An aliased or
  namespaced callee is left alone at every position, exactly as Vitest leaves
  it alone (`utilsObjectNames` defaults to `["vi", "vitest"]`). }
procedure TGocciaVitestMockHoister.VisitNode(const ANode: TGocciaASTNode;
  const AAtTopLevel: Boolean);
var
  BlockStatement: TGocciaBlockStatement;
  CaseClause: TGocciaCaseClause;
  CallExpression: TGocciaCallExpression;
  I, J: Integer;
  ObjectExpression: TGocciaObjectExpression;
  ObjectGetter: TGocciaGetterExpression;
  ObjectSetter: TGocciaSetterExpression;
  SwitchStatement: TGocciaSwitchStatement;
  TaggedTemplate: TGocciaTaggedTemplateExpression;
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
    VisitNode(TGocciaIfStatement(ANode).Condition, False);
    VisitNode(TGocciaIfStatement(ANode).Consequent, False);
    VisitNode(TGocciaIfStatement(ANode).Alternate, False);
  end
  else if ANode is TGocciaForStatement then
  begin
    VisitNode(TGocciaForStatement(ANode).Init, False);
    VisitNode(TGocciaForStatement(ANode).Condition, False);
    VisitNode(TGocciaForStatement(ANode).Update, False);
    VisitNode(TGocciaForStatement(ANode).Body, False);
  end
  else if ANode is TGocciaWhileStatement then
  begin
    VisitNode(TGocciaWhileStatement(ANode).Condition, False);
    VisitNode(TGocciaWhileStatement(ANode).Body, False);
  end
  else if ANode is TGocciaDoWhileStatement then
  begin
    VisitNode(TGocciaDoWhileStatement(ANode).Body, False);
    VisitNode(TGocciaDoWhileStatement(ANode).Condition, False);
  end
  else if ANode is TGocciaWithStatement then
  begin
    VisitNode(TGocciaWithStatement(ANode).ObjectExpression, False);
    VisitNode(TGocciaWithStatement(ANode).Body, False);
  end
  else if ANode is TGocciaForOfStatement then
  begin
    VisitNode(TGocciaForOfStatement(ANode).Iterable, False);
    VisitNode(TGocciaForOfStatement(ANode).Body, False);
  end
  else if ANode is TGocciaForInStatement then
  begin
    VisitNode(TGocciaForInStatement(ANode).ObjectExpression, False);
    VisitNode(TGocciaForInStatement(ANode).Body, False);
  end
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
    VisitNode(SwitchStatement.Discriminant, False);
    for I := 0 to SwitchStatement.Cases.Count - 1 do
    begin
      CaseClause := SwitchStatement.Cases[I];
      for J := 0 to CaseClause.Consequent.Count - 1 do
        VisitNode(CaseClause.Consequent[J], False);
    end;
  end
  else if ANode is TGocciaReturnStatement then
    VisitNode(TGocciaReturnStatement(ANode).Value, False)
  else if ANode is TGocciaThrowStatement then
    VisitNode(TGocciaThrowStatement(ANode).Value, False)
  else if ANode is TGocciaVariableDeclaration then
  begin
    VariableDeclaration := TGocciaVariableDeclaration(ANode);
    for I := Low(VariableDeclaration.Variables) to
             High(VariableDeclaration.Variables) do
      VisitNode(VariableDeclaration.Variables[I].Initializer, False);
  end
  else if ANode is TGocciaUsingDeclaration then
    for I := Low(TGocciaUsingDeclaration(ANode).Variables) to
             High(TGocciaUsingDeclaration(ANode).Variables) do
      VisitNode(TGocciaUsingDeclaration(ANode).Variables[I].Initializer, False)
  else if ANode is TGocciaDestructuringDeclaration then
    VisitNode(TGocciaDestructuringDeclaration(ANode).Initializer, False)
  else if ANode is TGocciaFunctionDeclaration then
    VisitNode(TGocciaFunctionDeclaration(ANode).FunctionExpression, False)
  else if ANode is TGocciaClassDeclaration then
    VisitClassDefinition(TGocciaClassDeclaration(ANode).ClassDefinition)
  else if ANode is TGocciaExportVariableDeclaration then
    VisitNode(TGocciaExportVariableDeclaration(ANode).Declaration, AAtTopLevel)
  else if ANode is TGocciaExportDestructuringDeclaration then
    VisitNode(TGocciaExportDestructuringDeclaration(ANode).Declaration, False)
  else if ANode is TGocciaExportFunctionDeclaration then
    VisitNode(TGocciaExportFunctionDeclaration(ANode).Declaration, False)
  else if ANode is TGocciaExportClassDeclaration then
    VisitNode(TGocciaExportClassDeclaration(ANode).Declaration, False)
  else if ANode is TGocciaExportDefaultDeclaration then
    VisitNode(TGocciaExportDefaultDeclaration(ANode).Expression, False)

  { Expressions. }
  else if ANode is TGocciaCallExpression then
  begin
    CallExpression := TGocciaCallExpression(ANode);
    CollectCall(CallExpression, AAtTopLevel);
    // The callee matters as much as the arguments: in `expect(f(cb)).toBe(x)`
    // everything but the trailing `.toBe(x)` sits inside the outer call's
    // callee, so a directive written in `cb` is only reachable through it.
    VisitNode(CallExpression.Callee, False);
    // A callback argument is where a nested directive realistically hides, as
    // in a `vi.mock` written inside a `describe` or `test` callback.
    VisitExpressions(CallExpression.Arguments);
  end
  else if ANode is TGocciaNewExpression then
  begin
    VisitNode(TGocciaNewExpression(ANode).Callee, False);
    VisitExpressions(TGocciaNewExpression(ANode).Arguments);
  end
  else if ANode is TGocciaMemberExpression then
  begin
    VisitNode(TGocciaMemberExpression(ANode).ObjectExpr, False);
    VisitNode(TGocciaMemberExpression(ANode).PropertyExpression, False);
  end
  else if ANode is TGocciaPrivateMemberExpression then
    VisitNode(TGocciaPrivateMemberExpression(ANode).ObjectExpr, False)
  else if ANode is TGocciaBinaryExpression then
  begin
    { `&&`, `||` and `??` are TGocciaBinaryExpression too, so this covers the
      logical forms as well. }
    VisitNode(TGocciaBinaryExpression(ANode).Left, False);
    VisitNode(TGocciaBinaryExpression(ANode).Right, False);
  end
  else if ANode is TGocciaConditionalExpression then
  begin
    VisitNode(TGocciaConditionalExpression(ANode).Condition, False);
    VisitNode(TGocciaConditionalExpression(ANode).Consequent, False);
    VisitNode(TGocciaConditionalExpression(ANode).Alternate, False);
  end
  else if ANode is TGocciaSequenceExpression then
    VisitExpressions(TGocciaSequenceExpression(ANode).Expressions)
  else if ANode is TGocciaUnaryExpression then
    VisitNode(TGocciaUnaryExpression(ANode).Operand, False)
  else if ANode is TGocciaIncrementExpression then
    VisitNode(TGocciaIncrementExpression(ANode).Operand, False)
  else if ANode is TGocciaAwaitExpression then
    VisitNode(TGocciaAwaitExpression(ANode).Operand, False)
  else if ANode is TGocciaYieldExpression then
    VisitNode(TGocciaYieldExpression(ANode).Operand, False)
  else if ANode is TGocciaSpreadExpression then
    VisitNode(TGocciaSpreadExpression(ANode).Argument, False)
  else if ANode is TGocciaAssignmentExpression then
    VisitNode(TGocciaAssignmentExpression(ANode).Value, False)
  else if ANode is TGocciaCompoundAssignmentExpression then
    VisitNode(TGocciaCompoundAssignmentExpression(ANode).Value, False)
  else if ANode is TGocciaPropertyAssignmentExpression then
  begin
    VisitNode(TGocciaPropertyAssignmentExpression(ANode).ObjectExpr, False);
    VisitNode(TGocciaPropertyAssignmentExpression(ANode).Value, False);
  end
  else if ANode is TGocciaPropertyCompoundAssignmentExpression then
  begin
    VisitNode(
      TGocciaPropertyCompoundAssignmentExpression(ANode).ObjectExpr, False);
    VisitNode(TGocciaPropertyCompoundAssignmentExpression(ANode).Value, False);
  end
  else if ANode is TGocciaComputedPropertyAssignmentExpression then
  begin
    VisitNode(
      TGocciaComputedPropertyAssignmentExpression(ANode).ObjectExpr, False);
    VisitNode(TGocciaComputedPropertyAssignmentExpression(ANode)
      .PropertyExpression, False);
    VisitNode(TGocciaComputedPropertyAssignmentExpression(ANode).Value, False);
  end
  else if ANode is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    VisitNode(TGocciaComputedPropertyCompoundAssignmentExpression(ANode)
      .ObjectExpr, False);
    VisitNode(TGocciaComputedPropertyCompoundAssignmentExpression(ANode)
      .PropertyExpression, False);
    VisitNode(TGocciaComputedPropertyCompoundAssignmentExpression(ANode)
      .Value, False);
  end
  else if ANode is TGocciaPrivatePropertyAssignmentExpression then
  begin
    VisitNode(
      TGocciaPrivatePropertyAssignmentExpression(ANode).ObjectExpr, False);
    VisitNode(TGocciaPrivatePropertyAssignmentExpression(ANode).Value, False);
  end
  else if ANode is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    VisitNode(TGocciaPrivatePropertyCompoundAssignmentExpression(ANode)
      .ObjectExpr, False);
    VisitNode(
      TGocciaPrivatePropertyCompoundAssignmentExpression(ANode).Value, False);
  end
  else if ANode is TGocciaDestructuringAssignmentExpression then
    VisitNode(TGocciaDestructuringAssignmentExpression(ANode).Right, False)
  else if ANode is TGocciaArrayExpression then
    VisitExpressions(TGocciaArrayExpression(ANode).Elements)
  else if ANode is TGocciaObjectExpression then
  begin
    ObjectExpression := TGocciaObjectExpression(ANode);
    for I := Low(ObjectExpression.PropertySourceOrder) to
             High(ObjectExpression.PropertySourceOrder) do
      VisitNode(ObjectExpression.PropertySourceOrder[I].Expression, False);
    for I := Low(ObjectExpression.ComputedPropertiesInOrder) to
             High(ObjectExpression.ComputedPropertiesInOrder) do
    begin
      VisitNode(ObjectExpression.ComputedPropertiesInOrder[I].Key, False);
      VisitNode(ObjectExpression.ComputedPropertiesInOrder[I].Value, False);
    end;
    { A getter or setter of an object literal is not carried by
      PropertySourceOrder[].Expression, only by these maps. }
    if Assigned(ObjectExpression.Getters) then
      for ObjectGetter in ObjectExpression.Getters.Values do
        if Assigned(ObjectGetter) then
          VisitNode(ObjectGetter.Body, False);
    if Assigned(ObjectExpression.Setters) then
      for ObjectSetter in ObjectExpression.Setters.Values do
        if Assigned(ObjectSetter) then
          VisitNode(ObjectSetter.Body, False);
  end
  else if ANode is TGocciaTemplateWithInterpolationExpression then
    VisitExpressions(TGocciaTemplateWithInterpolationExpression(ANode).Parts)
  else if ANode is TGocciaTaggedTemplateExpression then
  begin
    TaggedTemplate := TGocciaTaggedTemplateExpression(ANode);
    VisitNode(TaggedTemplate.Tag, False);
    VisitExpressions(TaggedTemplate.Expressions);
  end
  else if ANode is TGocciaImportCallExpression then
  begin
    VisitNode(TGocciaImportCallExpression(ANode).Specifier, False);
    VisitNode(TGocciaImportCallExpression(ANode).Options, False);
  end
  else if ANode is TGocciaClassExpression then
    VisitClassDefinition(TGocciaClassExpression(ANode).ClassDefinition)
  else if ANode is TGocciaObjectMethodDefinition then
    VisitNode(TGocciaObjectMethodDefinition(ANode).FunctionExpression, False)
  else if ANode is TGocciaGetterExpression then
    VisitNode(TGocciaGetterExpression(ANode).Body, False)
  else if ANode is TGocciaSetterExpression then
    VisitNode(TGocciaSetterExpression(ANode).Body, False)
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

function TGocciaVitestMockHoister.MockModules: TGocciaMockModules;
var
  I, Count: Integer;
begin
  { Mock and unmock apply in source order, last one wins — so an address whose
    final directive is an unmock keeps its real file, and a repeated mock keeps
    only the last factory. }
  SetLength(Result, FDirectiveCount);
  Count := 0;
  for I := 0 to FDirectiveCount - 1 do
  begin
    if FDirectives[I].Kind = vmdkUnmock then
      Continue;
    if not IsLastDirectiveForAddress(I) then
      Continue;
    Result[Count].Address := FDirectives[I].Address;
    Result[Count].Source := FDirectives[I].ModuleSource;
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

function CollectModuleMocks(const ASourceLines: TStringList;
  const ASourcePath: string; const APreprocessors: TGocciaPreprocessors;
  const AResolve: TGocciaMockResolveFunc): TGocciaMockModules;
var
  Hoister: TGocciaVitestMockHoister;
  PipelineOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  SourceText: string;
begin
  SetLength(Result, 0);
  if not Assigned(ASourceLines) then
    Exit;
  SourceText := StringListToSourceText(ASourceLines);
  { The overwhelming majority of files mention none of these, and a substring
    scan costs far less than the parse below. }
  if (Pos('vi.mock', SourceText) = 0) and (Pos('vi.unmock', SourceText) = 0) and
     (Pos('vitest.mock', SourceText) = 0) and
     (Pos('vitest.unmock', SourceText) = 0) then
    Exit;

  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := APreprocessors;
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
  { Both attempts swallow every exception class, not just TGocciaError. This
    pre-pass is best-effort by construction: the lexer, the preprocessors and
    the parser can all raise plain RTL exceptions (EConvertError,
    EArgumentException, EListError) on input the engine's own parse would
    reject anyway, and letting one escape here would kill a run over a file the
    hoister merely failed to analyse. A failure must therefore mean "no
    hoisting", never "crash". Nothing is lost by being quiet: the engine parses
    the same source immediately afterwards, and that parse — not this one — is
    the authoritative one that reports syntax errors to the user. }
  PipelineResult := nil;
  try
    PipelineOptions.SourceType := stModule;
    try
      PipelineResult := TGocciaSourcePipeline.Parse(ASourceLines,
        ASourcePath, PipelineOptions);
    except
      on E: Exception do
      begin
        if IsEngineIntegrityFault(E) then
          raise;
        PipelineResult := nil;
      end;
    end;

    if not Assigned(PipelineResult) then
    begin
      PipelineOptions.SourceType := stScript;
      try
        PipelineResult := TGocciaSourcePipeline.Parse(ASourceLines,
          ASourcePath, PipelineOptions);
      except
        on E: Exception do
        begin
          if IsEngineIntegrityFault(E) then
            raise;
          Exit;
        end;
      end;
    end;

    Hoister := TGocciaVitestMockHoister.Create(ASourcePath, AResolve);
    try
      Hoister.Collect(PipelineResult.ProgramNode);
      Result := Hoister.MockModules;
    finally
      Hoister.Free;
    end;
  finally
    PipelineResult.Free;
  end;
end;

end.
