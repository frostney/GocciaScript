{ The process-wide string encoding on Windows.

  GocciaScript strings are UTF-16, and FPC 3.2's RTL is not Unicode: every
  AnsiString it takes or returns (TFileStream and FindFirst file names,
  TStringList items, text written with WriteLn) goes through the system code
  page. On Linux and macOS that code page is UTF-8, so the round trip loses
  nothing. On Windows it is the ANSI code page (1252 on most systems), so a
  file name or an output line with a character outside it comes back with '?'.

  On Windows this unit makes UTF-8 the code page of those conversions, of the
  names the RTL returns from the file system, and of standard output and
  standard error, so Windows reads, lists and prints the same strings and the
  same bytes as the other platforms. A unit that converts strings through the
  RTL lists it in its uses, so that it is initialized first. }

unit ProcessEncoding;

{$I Shared.inc}

interface

implementation

{$IFDEF MSWINDOWS}
initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  SetTextCodePage(ErrOutput, CP_UTF8);
  SetTextCodePage(StdErr, CP_UTF8);
{$ENDIF}

end.
