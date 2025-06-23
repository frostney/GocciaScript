unit Goccia.Logger;

{$I Goccia.inc}

interface

uses
  SysUtils, TypInfo;

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
    procedure Trace(const Message: string); overload;
    procedure Trace(const Message: string; const Args: array of Const); overload;
    procedure Debug(const Message: string); overload;
    procedure Debug(const Message: string; const Args: array of Const); overload;
    procedure Info(const Message: string); overload;
    procedure Info(const Message: string; const Args: array of Const); overload;
    procedure Warn(const Message: string); overload;
    procedure Warn(const Message: string; const Args: array of Const); overload;
    procedure Error(const Message: string); overload;
    procedure Error(const Message: string; const Args: array of Const); overload;
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

procedure TGocciaLogger.Trace(const Message: string);
begin
  Self.Trace(Message, []);
end;

procedure TGocciaLogger.Trace(const Message: string; const Args: array of Const);
begin
  if llTrace in FLevels then
    WriteLn(Format('[TRACE] ' + Message, Args));
end;

procedure TGocciaLogger.Debug(const Message: string);
begin
  Self.Debug(Message, []);
end;

procedure TGocciaLogger.Debug(const Message: string; const Args: array of Const);
begin
  if llDebug in FLevels then
    WriteLn(Format('[DEBUG] ' + Message, Args));
end;

procedure TGocciaLogger.Info(const Message: string);
begin
  Self.Info(Message, []);
end;

procedure TGocciaLogger.Info(const Message: string; const Args: array of Const);
begin
  if llInfo in FLevels then
    WriteLn(Format('[INFO ] ' + Message, Args));
end;

procedure TGocciaLogger.Warn(const Message: string);
begin
  Self.Warn(Message, []);
end;

procedure TGocciaLogger.Warn(const Message: string; const Args: array of Const);
begin
  if llWarn in FLevels then
    WriteLn(Format('[WARN ] ' + Message, Args));
end;

procedure TGocciaLogger.Error(const Message: string);
begin
  Self.Error(Message, []);
end;

procedure TGocciaLogger.Error(const Message: string; const Args: array of Const);
begin
  if llError in FLevels then
    WriteLn(Format('[ERROR] ' + Message, Args));
end;

initialization
  Logger := TGocciaLogger.Create;

finalization
  Logger.Free;

end.
