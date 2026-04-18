unit Goccia.Executor;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node,
  Goccia.Evaluator.Context,
  Goccia.Modules.Loader,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaExecutor = class abstract
  private
    FCompileTimeNanoseconds: Int64;
    FExecuteTimeNanoseconds: Int64;
  protected
    FGlobalScope: TGocciaScope;
    FModuleLoader: TGocciaModuleLoader;
    FSourcePath: string;
  public
    procedure Initialize(const AGlobalScope: TGocciaScope;
      const AModuleLoader: TGocciaModuleLoader;
      const ASourcePath: string); virtual;
    function ExecuteProgram(
      const AProgram: TGocciaProgram): TGocciaValue; virtual; abstract;
    procedure EvaluateModuleBody(const AProgram: TGocciaProgram;
      const AContext: TGocciaEvaluationContext); virtual; abstract;
    procedure ClearTransientCaches; virtual;

    property CompileTimeNanoseconds: Int64 read FCompileTimeNanoseconds
      write FCompileTimeNanoseconds;
    property ExecuteTimeNanoseconds: Int64 read FExecuteTimeNanoseconds
      write FExecuteTimeNanoseconds;
  end;

implementation

procedure TGocciaExecutor.Initialize(const AGlobalScope: TGocciaScope;
  const AModuleLoader: TGocciaModuleLoader; const ASourcePath: string);
begin
  FGlobalScope := AGlobalScope;
  FModuleLoader := AModuleLoader;
  FSourcePath := ASourcePath;
end;

procedure TGocciaExecutor.ClearTransientCaches;
begin
  // Default: no-op
end;

end.
