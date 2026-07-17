unit Goccia.RuntimeExtensions.FFI;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.GlobalFFI,
  Goccia.Runtime;

type
  TGocciaFFIRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinFFI: TGocciaGlobalFFI;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  Goccia.Constants.ConstructorNames;

procedure TGocciaFFIRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinFFI := TGocciaGlobalFFI.Create(CONSTRUCTOR_FFI,
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError,
    Runtime.Engine.EmitCapabilityAudit);
  Runtime.RegisterRuntimeGlobalName('FFI');
end;

procedure TGocciaFFIRuntimeExtension.Detach;
begin
  FBuiltinFFI.Free;
  FBuiltinFFI := nil;
  inherited;
end;

end.
