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
  standard output and standard error, so Windows reads, lists and writes the
  same strings and the same bytes as the other platforms when output is
  redirected to a file or a pipe. When a standard handle is a console,
  ConsoleOutput writes its text to the console as UTF-16 with WriteConsoleW,
  so the console shows every character its font has, without a change to the
  console's code page. A unit that converts strings through the RTL lists this
  unit in its uses, so that it is initialized first.

  Standard output and standard error are thread variables in FPC, which the
  RTL opens again for each new thread with its defaults, so this applies to the
  main thread. The thread pool's workers capture their output for the main
  thread to write.

  On Linux and macOS this unit installs cwstring, which makes those
  conversions through iconv, with a direct path for ASCII text in front of it. }

unit ProcessEncoding;

{$I Shared.inc}

interface

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows,

  ConsoleOutput;

const
  { At most as many UTF-16 code units as UTF-8 bytes: a one-byte to
    three-byte character is one code unit, a four-byte one two. }
  CONSOLE_CHUNK_BYTES = 1024;

function IsConsoleHandle(const AHandle: THandle): Boolean;
var
  Mode: DWORD;
begin
  Result := GetConsoleMode(AHandle, Mode);
end;

function WriteConsoleCharacters(const AHandle: THandle;
  const ACharacters: PWideChar; const ACount: SizeInt): Boolean;
var
  Next: PWideChar;
  Left: SizeInt;
  Written: DWORD;
begin
  Next := ACharacters;
  Left := ACount;
  while Left > 0 do
  begin
    Written := 0;
    if not WriteConsoleW(AHandle, Next, Left, Written, nil) or
      (Written = 0) then
      Exit(False);
    Inc(Next, Written);
    Dec(Left, Written);
  end;
  Result := True;
end;

{ Converts UTF-8 to UTF-16 in a buffer on the stack, so that writing to the
  console allocates no memory, like the RTL's own write function: the runtime
  error message at exit can follow a failed allocation. A malformed byte
  becomes U+FFFD. }
function WriteConsoleUTF8(const AHandle: THandle; const AText: PAnsiChar;
  const ALength: SizeInt): Boolean;
var
  Characters: array[0..CONSOLE_CHUNK_BYTES - 1] of WideChar;
  Next: PAnsiChar;
  Left, Chunk, Count: SizeInt;
begin
  Next := AText;
  Left := ALength;
  while Left > 0 do
  begin
    Chunk := Left;
    if Chunk > CONSOLE_CHUNK_BYTES then
      Chunk := CompleteUTF8Length(Next, CONSOLE_CHUNK_BYTES);
    Count := MultiByteToWideChar(CP_UTF8, 0, Next, Chunk, @Characters[0],
      CONSOLE_CHUNK_BYTES);
    if (Count = 0) or
      not WriteConsoleCharacters(AHandle, @Characters[0], Count) then
      Exit(False);
    Inc(Next, Chunk);
    Dec(Left, Chunk);
  end;
  Result := True;
end;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  // A GUI application's StdOut and StdErr show a message box, and its other
  // standard files are not open, so they keep the RTL's settings.
  if IsConsole then
  begin
    UseUnicodeOutput(Output, IsConsoleHandle, WriteConsoleUTF8);
    UseUnicodeOutput(StdOut, IsConsoleHandle, WriteConsoleUTF8);
    UseUnicodeOutput(ErrOutput, IsConsoleHandle, WriteConsoleUTF8);
    UseUnicodeOutput(StdErr, IsConsoleHandle, WriteConsoleUTF8);
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
