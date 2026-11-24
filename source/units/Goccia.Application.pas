unit Goccia.Application;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaApplicationClass = class of TGocciaApplication;

  TGocciaApplication = class
  private
    FName: string;
  protected
    procedure Execute; virtual; abstract;
    procedure HandleError(const AException: Exception); virtual;
    { The process is about to free the application with AExitCode as its
      status (RunApplication, on every path out of Run). Default: nothing. }
    class procedure ProcessEnding(const AExitCode: LongInt); virtual;
    { The application is freed; the process ends after this. Default: nothing. }
    class procedure ProcessEnded; virtual;
    property Name: string read FName;
  public
    constructor Create(const AName: string); virtual;
    function Run: Integer;
    class function RunApplication(const AClass: TGocciaApplicationClass;
      const AName: string): Integer;
  end;

implementation

uses
  CLI.Options,

  Goccia.Error,
  Goccia.Modules.Resolver,
  Goccia.Terminal.Colors;

constructor TGocciaApplication.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

procedure TGocciaApplication.HandleError(const AException: Exception);
var
  UseColor: Boolean;
begin
  UseColor := IsColorTerminal;
  if AException is TGocciaError then
    WriteLn(FormatHostErrorDiagnostic(TGocciaError(AException), UseColor))
  else
    WriteLn('Error: ', AException.Message);
end;

class procedure TGocciaApplication.ProcessEnding(const AExitCode: LongInt);
begin
end;

class procedure TGocciaApplication.ProcessEnded;
begin
end;

function TGocciaApplication.Run: Integer;
begin
  Result := 0;
  try
    Execute;
  except
    { An unusable invocation: the command did no work (CLI conventions). }
    on E: TCLIUsageError do
    begin
      WriteLn(ErrOutput, 'Error: ', E.Message);
      Result := EXIT_CODE_USAGE;
    end;
    on E: Exception do
    begin
      HandleError(E);
      Result := 1;
    end;
  end;
end;

class function TGocciaApplication.RunApplication(
  const AClass: TGocciaApplicationClass; const AName: string): Integer;
var
  App: TGocciaApplication;
begin
  Result := 1;
  App := AClass.Create(AName);
  try
    Result := App.Run;
  finally
    if Result <> 0 then
      AClass.ProcessEnding(Result)
    else
      AClass.ProcessEnding(ExitCode);
    App.Free;
    AClass.ProcessEnded;
  end;
end;

end.
