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
  SysUtils,

  Goccia.Engine,
  Goccia.Modules.Virtual,
  Goccia.Vitest.MockHoist;

function VitestCompatShimSource: string;
const
  LB = sLineBreak;
begin
  {$I Goccia.VitestShim.inc}
end;

type
  { the engine's module loader as a mock resolver }
  TGocciaVitestMockResolver = class
  private
    FEngine: TGocciaEngine;
  public
    constructor Create(const AEngine: TGocciaEngine);
    function Resolve(const ASpecifier, AReferrer: string;
      out AAddress: string): Boolean;
  end;

constructor TGocciaVitestMockResolver.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
end;

function TGocciaVitestMockResolver.Resolve(const ASpecifier, AReferrer: string;
  out AAddress: string): Boolean;
begin
  AAddress := FEngine.ModuleLoader.ResolveModuleAddress(ASpecifier, AReferrer);
  Result := AAddress <> '';
end;

{ Finds the `vi.mock` / `vi.unmock` calls in the entry source and turns them
  into virtual modules before the engine parses that source for real. }
procedure HoistModuleMocks(const AEngine: TGocciaEngine);
var
  Resolver: TGocciaVitestMockResolver;
  Mocks: TGocciaMockModules;
  I: Integer;
begin
  Resolver := TGocciaVitestMockResolver.Create(AEngine);
  try
    Mocks := CollectModuleMocks(AEngine.SourceLines, AEngine.SourcePath,
      AEngine.Preprocessors, Resolver.Resolve);
  finally
    Resolver.Free;
  end;
  for I := 0 to High(Mocks) do
    AEngine.ModuleLoader.InjectModule(Mocks[I].Address, Mocks[I].Source,
      'javascript', '', VIRTUAL_MODULE_PROVENANCE_VITEST_MOCK);
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
