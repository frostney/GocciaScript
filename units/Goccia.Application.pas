unit Goccia.Application;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Engine;

type
  TGocciaApplicationClass = class of TGocciaApplication;

  TGocciaApplication = class
  private
    FName: string;
  protected
    procedure Execute; virtual; abstract;
    procedure HandleError(const AException: Exception); virtual;
    function GlobalBuiltins: TGocciaGlobalBuiltins; virtual;
    property Name: string read FName;
  public
    constructor Create(const AName: string);
    function Run: Integer;
    class function RunApplication(const AClass: TGocciaApplicationClass;
      const AName: string): Integer;
  end;

implementation

uses
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.GarbageCollector,
  Goccia.Terminal.Colors,
  Goccia.Values.Error,
  Goccia.VM.Exception;

constructor TGocciaApplication.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TGocciaApplication.GlobalBuiltins: TGocciaGlobalBuiltins;
begin
  Result := [];
end;

procedure TGocciaApplication.HandleError(const AException: Exception);
var
  UseColor: Boolean;
begin
  UseColor := IsColorTerminal;
  if AException is TGocciaError then
    WriteLn(TGocciaError(AException).GetDetailedMessage(UseColor))
  else if AException is TGocciaThrowValue then
    WriteLn(FormatThrowDetail(TGocciaThrowValue(AException).Value, '', nil, UseColor))
  else if AException is EGocciaBytecodeThrow then
    WriteLn(FormatThrowDetail(EGocciaBytecodeThrow(AException).ThrownValue, '', nil, UseColor))
  else
    WriteLn('Error: ', AException.Message);
end;

function TGocciaApplication.Run: Integer;
begin
  Result := 0;
  TGarbageCollector.Initialize;
  try
    try
      Execute;
    except
      on E: Exception do
      begin
        HandleError(E);
        Result := 1;
      end;
    end;
  finally
    TGarbageCollector.Shutdown;
  end;
end;

class function TGocciaApplication.RunApplication(
  const AClass: TGocciaApplicationClass; const AName: string): Integer;
var
  App: TGocciaApplication;
begin
  App := AClass.Create(AName);
  try
    Result := App.Run;
  finally
    App.Free;
  end;
end;

end.
