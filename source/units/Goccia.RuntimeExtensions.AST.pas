unit Goccia.RuntimeExtensions.AST;

{$I Goccia.inc}

interface

uses
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

type
  { Experimental. The module carries no capability — it reads a string the
    caller already holds — but its shape is not settled, so hosts install it
    behind `--experimental-ast` rather than in the loader profile. See
    ADR 0117. }
  TGocciaASTRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FASTModule: TGocciaRuntimeNamespaceModuleRegistration;
    function MaterializeAST: TGocciaValue;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  Goccia.Builtins.AST;

procedure TGocciaASTRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FASTModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:ast',
    MaterializeAST);
end;

procedure TGocciaASTRuntimeExtension.Detach;
begin
  FASTModule.Free;
  FASTModule := nil;
  inherited;
end;

function TGocciaASTRuntimeExtension.MaterializeAST: TGocciaValue;
begin
  Result := CreateASTNamespace;
end;

end.
