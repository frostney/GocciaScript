{ The process-wide string encoding on Windows.

  GocciaScript strings are UTF-16, and FPC 3.2's RTL is not Unicode: every
  AnsiString it takes or returns (TFileStream and FindFirst file names,
  TStringList items, text written with WriteLn) goes through the system code
  page. Under the usual UTF-8 locale on Linux and macOS that code page is
  UTF-8, so the round trip loses nothing. On Windows it is the ANSI code page
  (1252 on most systems), so a file name or an output line with a character
  outside it comes back with '?'.

  On Windows this unit makes UTF-8 the code page of those conversions and of
  the names the RTL returns from the file system, and the code page of
  standard output and standard error when they are redirected to a file or a
  pipe, so Windows reads, lists and writes the same strings and the same bytes
  as the other platforms. A console keeps the console's output code page, which
  the RTL selects, so it displays what it displayed before; a character outside
  that code page still prints as '?'. A unit that converts strings through the
  RTL lists this unit in its uses, so that it is initialized first. }

unit ProcessEncoding;

{$I Shared.inc}

interface

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;

function IsConsole(const AStdHandle: DWORD): Boolean;
var
  Mode: DWORD;
begin
  Result := GetConsoleMode(GetStdHandle(AStdHandle), Mode);
end;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  if not IsConsole(STD_OUTPUT_HANDLE) then
    SetTextCodePage(Output, CP_UTF8);
  if not IsConsole(STD_ERROR_HANDLE) then
  begin
    SetTextCodePage(ErrOutput, CP_UTF8);
    SetTextCodePage(StdErr, CP_UTF8);
  end;
{$ENDIF}

end.
