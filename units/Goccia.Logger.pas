unit Goccia.Logger;

{$I Goccia.inc}

interface

uses
  SysUtils,
  TypInfo;

type
  TGocciaLogLevel = (llTrace, llDebug, llInfo, llWarn, llError);
  TGocciaLogLevels = set of TGocciaLogLevel;
  TGocciaLogFormat = (lfConsole, lfJson);

  TGocciaLogger = class
  private
    FLevels: TGocciaLogLevels;
    FLogFormat: TGocciaLogFormat;
  public
    constructor Create(const ALevels: TGocciaLogLevels = [llInfo, llWarn, llError]; const ALogFormat: TGocciaLogFormat = lfConsole);
    procedure Trace(const AMessage: string); overload;
    procedure Trace(const AMessage: string; const AArgs: array of Const); overload;
    procedure Debug(const AMessage: string); overload;
    procedure Debug(const AMessage: string; const AArgs: array of Const); overload;
    procedure Info(const AMessage: string); overload;
    procedure Info(const AMessage: string; const AArgs: array of Const); overload;
    procedure Warn(const AMessage: string); overload;
    procedure Warn(const AMessage: string; const AArgs: array of Const); overload;
    procedure Error(const AMessage: string); overload;
    procedure Error(const AMessage: string; const AArgs: array of Const); overload;
    property Levels: TGocciaLogLevels read FLevels write FLevels;
    property LogFormat: TGocciaLogFormat read FLogFormat write FLogFormat;
  end;

var
  Logger: TGocciaLogger;

implementation

constructor TGocciaLogger.Create(const ALevels: TGocciaLogLevels; const ALogFormat: TGocciaLogFormat);
var
  Levels: String;
  Level: TGocciaLogLevel;
begin
  FLevels := ALevels;
  FLogFormat := ALogFormat;

  Levels := '';
  for Level in ALevels do
    Levels := Levels + GetEnumName(TypeInfo(TGocciaLogLevel), Ord(Level)) + ', ';

  WriteLn(Format('[LOGGER] Logger created with levels: %s and format: %s', [Levels, GetEnumName(TypeInfo(TGocciaLogFormat), Ord(FLogFormat))]));
end;

procedure TGocciaLogger.Trace(const AMessage: string);
begin
  Self.Trace(AMessage, []);
end;

procedure TGocciaLogger.Trace(const AMessage: string; const AArgs: array of Const);
begin
  if llTrace in FLevels then
    WriteLn(Format('[TRACE] ' + AMessage, AArgs));
end;

procedure TGocciaLogger.Debug(const AMessage: string);
begin
  Self.Debug(AMessage, []);
end;

procedure TGocciaLogger.Debug(const AMessage: string; const AArgs: array of Const);
begin
  if llDebug in FLevels then
    WriteLn(Format('[DEBUG] ' + AMessage, AArgs));
end;

procedure TGocciaLogger.Info(const AMessage: string);
begin
  Self.Info(AMessage, []);
end;

procedure TGocciaLogger.Info(const AMessage: string; const AArgs: array of Const);
begin
  if llInfo in FLevels then
    WriteLn(Format('[INFO ] ' + AMessage, AArgs));
end;

procedure TGocciaLogger.Warn(const AMessage: string);
begin
  Self.Warn(AMessage, []);
end;

procedure TGocciaLogger.Warn(const AMessage: string; const AArgs: array of Const);
begin
  if llWarn in FLevels then
    WriteLn(Format('[WARN ] ' + AMessage, AArgs));
end;

procedure TGocciaLogger.Error(const AMessage: string);
begin
  Self.Error(AMessage, []);
end;

procedure TGocciaLogger.Error(const AMessage: string; const AArgs: array of Const);
begin
  if llError in FLevels then
    WriteLn(Format('[ERROR] ' + AMessage, AArgs));
end;

initialization
  Logger := TGocciaLogger.Create;

finalization
  Logger.Free;

end.
