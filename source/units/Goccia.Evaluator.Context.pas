unit Goccia.Evaluator.Context;

{$I Goccia.inc}

interface

uses
  Goccia.Error.ThrowErrorCallback,
  Goccia.Modules,
  Goccia.Scope;

type
  TGocciaEvaluationContext = record
    Scope: TGocciaScope;
    OnError: TGocciaThrowErrorCallback;
    LoadModule: TLoadModuleCallback;
    CurrentFilePath: string;
    CoverageEnabled: Boolean;
    StrictTypes: Boolean;
    DisposalTracker: TObject; // TGocciaDisposalTracker or nil
  end;

implementation

end.
