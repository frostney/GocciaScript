unit Goccia.Executor;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node,
  Goccia.Evaluator.Context,
  Goccia.Modules.Loader,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaCompiledModule = class
  end;

  TRetainObjectCallback = procedure(const AObject: TObject) of object;

  TGocciaExecutor = class abstract
  private
    FCompileTimeNanoseconds: Int64;
    FExecuteTimeNanoseconds: Int64;
  protected
    FGlobalScope: TGocciaScope;
    FModuleLoader: TGocciaModuleLoader;
    FRealm: TGocciaRealm;
    FSourcePath: string;
    FRetainModule: TRetainObjectCallback;
  public
    procedure Initialize(const AGlobalScope: TGocciaScope;
      const AModuleLoader: TGocciaModuleLoader;
      const ASourcePath: string); virtual;
    function ExecuteProgram(
      const AProgram: TGocciaProgram): TGocciaValue; virtual; abstract;
    { Evaluate the module body in AContext and return the completion
      value (last expression value, or undefined). }
    function EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext;
      out AProgramConsumed: Boolean): TGocciaValue;
      virtual; abstract;
    function ExecuteDynamicFunction(
      const AProgram: TGocciaProgram): TGocciaValue; virtual; abstract;
    function CompileModule(
      const AProgram: TGocciaProgram): TGocciaCompiledModule; virtual; abstract;
    function RunCompiledModule(
      const AModule: TGocciaCompiledModule): TGocciaValue; virtual; abstract;
    function RunCompiledModuleInScope(
      const AModule: TGocciaCompiledModule;
      const AScope: TGocciaScope): TGocciaValue; virtual; abstract;
    procedure ClearTransientCaches; virtual;

    property CompileTimeNanoseconds: Int64 read FCompileTimeNanoseconds
      write FCompileTimeNanoseconds;
    property ExecuteTimeNanoseconds: Int64 read FExecuteTimeNanoseconds
      write FExecuteTimeNanoseconds;
    property Realm: TGocciaRealm read FRealm;
    property RetainModuleCallback: TRetainObjectCallback
      read FRetainModule write FRetainModule;
  end;

implementation

procedure TGocciaExecutor.Initialize(const AGlobalScope: TGocciaScope;
  const AModuleLoader: TGocciaModuleLoader; const ASourcePath: string);
begin
  FGlobalScope := AGlobalScope;
  FModuleLoader := AModuleLoader;
  FRealm := CurrentRealm;
  FSourcePath := ASourcePath;
end;

procedure TGocciaExecutor.ClearTransientCaches;
begin
  // Default: no-op
end;

end.
