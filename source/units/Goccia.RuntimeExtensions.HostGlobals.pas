unit Goccia.RuntimeExtensions.HostGlobals;

{$I Goccia.inc}

interface

uses
  Goccia.Runtime;

type
  TGocciaHostGlobalsRuntimeExtension = class(TGocciaRuntimeExtension)
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
  end;

implementation

procedure TGocciaHostGlobalsRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  Runtime.Engine.BuiltinGlobals.RegisterRuntimeGlobals;
end;

end.
