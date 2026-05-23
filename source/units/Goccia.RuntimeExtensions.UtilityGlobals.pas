unit Goccia.RuntimeExtensions.UtilityGlobals;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaUtilityGlobalsRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

procedure TGocciaUtilityGlobalsRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  Runtime.Engine.BuiltinGlobals.RegisterUtilityRuntimeGlobals;
end;

end.
