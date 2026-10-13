unit Goccia.Evaluator.Context;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Modules,
  Goccia.Realm,
  Goccia.Scope;

type
  TGocciaEvalRejectNameArray = array of string;

  TGocciaEvaluationContext = record
    Realm: TGocciaRealm;
    Scope: TGocciaScope;
    OnError: TGocciaThrowErrorCallback;
    LoadModule: TLoadModuleCallback;
    LoadModuleSource: TLoadModuleSourceCallback;
    LoadDeferredModule: TLoadDeferredModuleCallback;
    ResolveModuleURL: TResolveModuleURLCallback;
    CurrentFilePath: string;
    CoverageEnabled: Boolean;
    StrictTypes: Boolean;
    NonStrictMode: Boolean;
    CompatibilityNonStrictMode: Boolean;
    HideFunctionSourceText: Boolean;
    InEvalCode: Boolean;
    EvalVarScope: TGocciaScope;
    RejectArgumentsVarDeclarationInEval: Boolean;
    RejectVarDeclarationNamesInEval: TGocciaEvalRejectNameArray;
    DisposalTracker: TObject; // TGocciaDisposalTracker or nil
    CurrentModule: TGocciaModule;
    ModuleEnvironmentInitialized: Boolean;
    { The module body an executor compiled while it instantiated the
      module's function declarations (TGocciaModuleFunctionInstantiator),
      which evaluating the body then runs; nil when nothing was compiled.
      A TGocciaBytecodeModule, typed as TObject to keep the bytecode units
      out of this one. }
    CompiledModule: TObject;
  end;

implementation

end.
