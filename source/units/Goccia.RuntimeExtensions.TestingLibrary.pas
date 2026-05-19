unit Goccia.RuntimeExtensions.TestingLibrary;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.TestingLibrary,
  Goccia.Runtime;

type
  TGocciaTestingLibraryRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FBuiltinTestAssertions: TGocciaTestAssertions;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

procedure TGocciaTestingLibraryRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
begin
  inherited Attach(ARuntime);
  FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions',
    Runtime.Engine.Interpreter.GlobalScope, Runtime.Engine.ThrowError);
  Runtime.RegisterRuntimeBuiltinName('TestAssertions');
end;

procedure TGocciaTestingLibraryRuntimeExtension.Detach;
begin
  FBuiltinTestAssertions.Free;
  FBuiltinTestAssertions := nil;
  inherited;
end;

end.
