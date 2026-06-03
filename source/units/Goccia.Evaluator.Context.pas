unit Goccia.Evaluator.Context;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Modules,
  Goccia.Realm,
  Goccia.Scope;

type
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
    DisposalTracker: TObject; // TGocciaDisposalTracker or nil
  end;

implementation

end.
