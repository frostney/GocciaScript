unit Goccia.RuntimeExtensions.Semver;

{$I Goccia.inc}

interface

uses
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.NamespaceModule,
  Goccia.Values.Primitives;

type
  TGocciaSemverRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FSemverModule: TGocciaRuntimeNamespaceModuleRegistration;
    function MaterializeSemver: TGocciaValue;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  Goccia.Builtins.Semver;

procedure TGocciaSemverRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FSemverModule := TGocciaRuntimeNamespaceModuleRegistration.Create(Runtime,
    'goccia:semver',
    MaterializeSemver);
end;

procedure TGocciaSemverRuntimeExtension.Detach;
begin
  FSemverModule.Free;
  FSemverModule := nil;
  inherited;
end;

function TGocciaSemverRuntimeExtension.MaterializeSemver: TGocciaValue;
begin
  Result := CreateSemverNamespace;
end;

end.
