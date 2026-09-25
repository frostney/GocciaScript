unit Goccia.RuntimeExtensions.AsyncHooks;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

const
  { Node's own address for the module, so a package written for Node imports it
    unchanged. There is no `goccia:` spelling: the surface is Node's, not one
    GocciaScript invented. }
  ASYNC_HOOKS_MODULE_NAME = 'node:async_hooks';

type
  TGocciaAsyncHooksRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FAsyncHooksModule: TGocciaRuntimeNamespaceModuleRegistration;
    FHostToken: TObject;
    function MaterializeAsyncHooks: TGocciaValue;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  Goccia.Builtins.AsyncHooks;

procedure TGocciaAsyncHooksRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FAsyncHooksModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    ASYNC_HOOKS_MODULE_NAME,
    MaterializeAsyncHooks,
    True);
end;

procedure TGocciaAsyncHooksRuntimeExtension.Detach;
begin
  FAsyncHooksModule.Free;
  FAsyncHooksModule := nil;
  { Release this extension's own host state rather than leaving it for thread
    teardown: several engines can live on one thread, and a detached one must
    not keep its prototypes and namespace alive until the thread ends. }
  ReleaseAsyncHooksHost(FHostToken);
  FHostToken := nil;
  inherited;
end;

function TGocciaAsyncHooksRuntimeExtension.MaterializeAsyncHooks: TGocciaValue;
begin
  Result := CreateAsyncHooksNamespace(FHostToken);
end;

end.
