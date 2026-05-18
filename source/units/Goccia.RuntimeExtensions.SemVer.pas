unit Goccia.RuntimeExtensions.SemVer;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaSemVerRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

uses
  Goccia.Builtins.Semver,
  Goccia.Constants.PropertyNames;

procedure TGocciaSemVerRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  if Assigned(Runtime.Engine.GocciaGlobal) then
    Runtime.Engine.GocciaGlobal.AssignProperty(
      SEMVER_NAMESPACE_PROPERTY, CreateSemverNamespace);
end;

end.
