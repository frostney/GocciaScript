unit Goccia.Terminal.Colors;

{$I Goccia.inc}

interface

const
  ANSI_RED = #27'[31m';
  ANSI_GREEN = #27'[32m';
  ANSI_YELLOW = #27'[33m';
  ANSI_CYAN = #27'[36m';
  ANSI_GRAY = #27'[90m';
  ANSI_BOLD = #27'[1m';
  ANSI_DIM = #27'[2m';
  ANSI_RESET = #27'[0m';

function IsColorTerminal: Boolean;
function Colorize(const AText, AColor: string; const AUseColor: Boolean): string; inline;

implementation

{$IFDEF UNIX}
uses
  termio;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

function IsColorTerminal: Boolean;
{$IFDEF MSWINDOWS}
const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004;
var
  Handle: THandle;
  Mode: DWORD;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Result := IsATTY(StdOutputHandle) <> 0;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  if Handle = INVALID_HANDLE_VALUE then
    Exit(False);
  if not GetConsoleMode(Handle, Mode) then
    Exit(False);
  // Enable ANSI escape processing on Windows 10+
  if (Mode and ENABLE_VIRTUAL_TERMINAL_PROCESSING) = 0 then
    SetConsoleMode(Handle, Mode or ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
  {$ENDIF}
end;

function Colorize(const AText, AColor: string; const AUseColor: Boolean): string;
begin
  if AUseColor then
    Result := AColor + AText + ANSI_RESET
  else
    Result := AText;
end;

end.
