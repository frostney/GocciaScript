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

  Goccia.EngineFault,
  Goccia.Error,
  {$IFDEF GOCCIA_FAULT_INJECTION}
  Goccia.FaultInjection,
  {$ENDIF}
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
    {$IFDEF GOCCIA_FAULT_INJECTION}
    InjectFaultAt('startup');
    {$ENDIF}
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
      { a fault the application's own handlers did not stop at: raised on the
        main thread, outside any file (ADR 0201) }
      if IsEngineFault(E) then
        AbortOnEngineFault(FName, E);
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
  { Run stops on a fault inside Execute; this handler stops on one in the
    application's construction, its ending and its destruction (ADR 0201) }
  try
    App := AClass.Create(AName);
    try
      Result := App.Run;
    finally
      {$IFDEF GOCCIA_FAULT_INJECTION}
      InjectFaultAt('shutdown');
      {$ENDIF}
      if Result <> 0 then
        AClass.ProcessEnding(Result)
      else
        AClass.ProcessEnding(ExitCode);
      App.Free;
      AClass.ProcessEnded;
    end;
  except
    on E: Exception do
    begin
      if IsEngineFault(E) then
        AbortOnEngineFault(AName, E);
      raise;
    end;
  end;
end;

end.
