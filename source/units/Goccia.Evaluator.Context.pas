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
    CurrentFilePath: string;
    CoverageEnabled: Boolean;
    StrictTypes: Boolean;
    NonStrictMode: Boolean;
    InEvalCode: Boolean;
    RejectArgumentsVarDeclarationInEval: Boolean;
    RejectVarDeclarationNamesInEval: TGocciaEvalRejectNameArray;
    DisposalTracker: TObject; // TGocciaDisposalTracker or nil
  end;

implementation

end.
