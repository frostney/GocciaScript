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
    CurrentFilePath: string;
    CoverageEnabled: Boolean;
    StrictTypes: Boolean;
    NonStrictMode: Boolean;
    CompatibilityNonStrictMode: Boolean;
    InEvalCode: Boolean;
    EvalVarScope: TGocciaScope;
    RejectArgumentsVarDeclarationInEval: Boolean;
    RejectVarDeclarationNamesInEval: TGocciaEvalRejectNameArray;
    DisposalTracker: TObject; // TGocciaDisposalTracker or nil
    CurrentModule: TGocciaModule;
    ModuleEnvironmentInitialized: Boolean;
  end;

implementation

end.
