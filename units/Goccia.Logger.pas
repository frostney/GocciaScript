unit Goccia.Logger;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaLogger = class
  public
    class procedure Debug(const Message: string); overload;
    class procedure Debug(const Message: string; const Args: array of Const); overload;
    class procedure Info(const Message: string); overload;
    class procedure Info(const Message: string; const Args: array of Const); overload;
    class procedure Warn(const Message: string); overload;
    class procedure Warn(const Message: string; const Args: array of Const); overload;
    class procedure Error(const Message: string); overload;
    class procedure Error(const Message: string; const Args: array of Const); overload;
  end;

implementation

class procedure TGocciaLogger.Debug(const Message: string);
begin
  TGocciaLogger.Debug(Message, []);
end;

class procedure TGocciaLogger.Debug(const Message: string; const Args: array of Const);
begin
  WriteLn(Format('[DEBUG] ' + Message, Args));
end;

class procedure TGocciaLogger.Info(const Message: string);
begin
  TGocciaLogger.Info(Message, []);
end;

class procedure TGocciaLogger.Info(const Message: string; const Args: array of Const);
begin
  WriteLn(Format('[INFO ] ' + Message, Args));
end;

class procedure TGocciaLogger.Warn(const Message: string);
begin
  TGocciaLogger.Warn(Message, []);
end;

class procedure TGocciaLogger.Warn(const Message: string; const Args: array of Const);
begin
  WriteLn(Format('[WARN ] ' + Message, Args));
end;

class procedure TGocciaLogger.Error(const Message: string);
begin
  TGocciaLogger.Error(Message, []);
end;

class procedure TGocciaLogger.Error(const Message: string; const Args: array of Const);
begin
  WriteLn(Format('[ERROR] ' + Message, Args));
end;

end.