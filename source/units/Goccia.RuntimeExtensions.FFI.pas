unit Goccia.RuntimeExtensions.FFI;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Builtins.GlobalFFI,
  Goccia.Runtime;

type
  { Installs the FFI global. Refuses to attach to an engine whose capability
    set does not grant ffi: without the grant the global stays absent, so
    feature detection keeps working (ADR 0122). }
  TGocciaFFIRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinFFI: TGocciaGlobalFFI;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

  EGocciaFFINotGranted = class(Exception);

{ Installs the FFI extension when the runtime's engine grants ffi; returns
  whether it did. The one entry point hosts use instead of deciding for
  themselves. }
function InstallFFIIfGranted(const ARuntime: TGocciaRuntimeCore): Boolean;

implementation

uses
  Goccia.Capabilities,
  Goccia.Constants.ConstructorNames;

function InstallFFIIfGranted(const ARuntime: TGocciaRuntimeCore): Boolean;
begin
  Result := Assigned(ARuntime) and ARuntime.Engine.Capabilities.Grants(gcFFI);
  if Result then
    ARuntime.Install(TGocciaFFIRuntimeExtension.Create);
end;

procedure TGocciaFFIRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  if Assigned(ARuntime) and
     not ARuntime.Engine.Capabilities.Grants(gcFFI) then
    raise EGocciaFFINotGranted.Create(
      'The FFI runtime extension requires an engine that grants the ffi ' +
      'capability.');
  inherited Attach(ARuntime);
  FBuiltinFFI := TGocciaGlobalFFI.Create(CONSTRUCTOR_FFI,
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    Runtime.Engine.Capabilities, Runtime.Engine.EmitCapabilityAudit);
  Runtime.RegisterRuntimeGlobalName('FFI');
end;

procedure TGocciaFFIRuntimeExtension.Detach;
begin
  FBuiltinFFI.Free;
  FBuiltinFFI := nil;
  inherited;
end;

end.
