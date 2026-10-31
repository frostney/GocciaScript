{ The process-wide string encoding.

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
  RTL lists this unit in its uses, so that it is initialized first.

  On Linux and macOS this unit installs cwstring, which makes those
  conversions through iconv, with a direct path for ASCII text in front of it. }

unit ProcessEncoding;

{$I Shared.inc}

interface

implementation

{$IF DEFINED(MSWINDOWS)}
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
{$ELSEIF DEFINED(FPC) AND DEFINED(UNIX)}
uses
  cwstring;

{ cwstring converts between UTF-16 and the system code page through iconv,
  which costs a converter lookup and a call per string. Most strings that
  cross the boundary are ASCII — resource strings, identifiers, file names —
  and ASCII is the same bytes in UTF-8 and in ASCII, so those are converted
  here and everything else is handed to cwstring as before. }
var
  GIconvUnicode2AnsiMove: procedure(ASource: PUnicodeChar;
    var ADest: RawByteString; ACodePage: TSystemCodePage; ALength: SizeInt);
  GIconvAnsi2UnicodeMove: procedure(ASource: PAnsiChar;
    ACodePage: TSystemCodePage; var ADest: UnicodeString; ALength: SizeInt);

function IsASCIICompatibleCodePage(const ACodePage: TSystemCodePage): Boolean;
begin
  Result := (ACodePage = CP_UTF8) or (ACodePage = CP_ASCII);
end;

procedure ASCIIUnicode2AnsiMove(ASource: PUnicodeChar;
  var ADest: RawByteString; ACodePage: TSystemCodePage; ALength: SizeInt);
var
  Target: PAnsiChar;
  I: SizeInt;
begin
  if IsASCIICompatibleCodePage(ACodePage) then
  begin
    I := 0;
    while (I < ALength) and (Ord(ASource[I]) <= $7F) do
      Inc(I);
    if I = ALength then
    begin
      SetLength(ADest, ALength);
      Target := PAnsiChar(ADest);
      for I := 0 to ALength - 1 do
        Target[I] := AnsiChar(Ord(ASource[I]));
      SetCodePage(ADest, ACodePage, False);
      Exit;
    end;
  end;
  GIconvUnicode2AnsiMove(ASource, ADest, ACodePage, ALength);
end;

procedure ASCIIAnsi2UnicodeMove(ASource: PAnsiChar; ACodePage: TSystemCodePage;
  var ADest: UnicodeString; ALength: SizeInt);
var
  Target: PUnicodeChar;
  I: SizeInt;
begin
  if IsASCIICompatibleCodePage(ACodePage) then
  begin
    I := 0;
    while (I < ALength) and (Ord(ASource[I]) <= $7F) do
      Inc(I);
    if I = ALength then
    begin
      SetLength(ADest, ALength);
      Target := PUnicodeChar(ADest);
      for I := 0 to ALength - 1 do
        Target[I] := UnicodeChar(Ord(ASource[I]));
      Exit;
    end;
  end;
  GIconvAnsi2UnicodeMove(ASource, ACodePage, ADest, ALength);
end;

procedure InstallASCIIConversions;
begin
  GIconvUnicode2AnsiMove := WideStringManager.Unicode2AnsiMoveProc;
  GIconvAnsi2UnicodeMove := WideStringManager.Ansi2UnicodeMoveProc;
  WideStringManager.Unicode2AnsiMoveProc := ASCIIUnicode2AnsiMove;
  WideStringManager.Ansi2UnicodeMoveProc := ASCIIAnsi2UnicodeMove;
end;

initialization
  InstallASCIIConversions;
{$ENDIF}

end.
