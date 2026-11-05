{ Unicode text on standard output and standard error.

  FPC 3.2's RTL writes a UnicodeString to a text file by converting it to the
  file's code page. A Windows console starts in the console's output code page
  (437 or 850, for example), in which a character outside that page becomes
  '?'. Only a console accepts UTF-16 through WriteConsoleW, and a file or a
  pipe wants the UTF-8 bytes the other platforms write.

  UseUnicodeOutput makes a standard text file write UTF-8, and when its handle
  is a console, it also replaces the file's write function: the RTL fills the
  buffer with UTF-8, and each flush converts the complete characters in the
  buffer to UTF-16 and hands them to the writer, which writes them to the
  console. A character split across two flushes, when a long string fills the
  buffer, stays at the start of the buffer until the rest of it arrives. Every
  Write and WriteLn to the file goes through it, so nothing that writes to
  standard output changes. The console's code page is not changed.

  The probe and the writer are parameters so that the logic runs on every
  platform under test; ProcessEncoding passes GetConsoleMode and WriteConsoleW
  on Windows. }

unit ConsoleOutput;

{$I Shared.inc}

interface

type
  { True when AHandle is a console. }
  TConsoleProbe = function(const AHandle: THandle): Boolean;
  { Writes ALength UTF-16 code units to the console AHandle; False on failure. }
  TConsoleUTF16Writer = function(const AHandle: THandle;
    const AText: PUnicodeChar; const ALength: SizeInt): Boolean;

{ Makes AOutput, an open standard text file, write UTF-8: to a console as
  UTF-16 through AWriter, to anything else as UTF-8 bytes. }
procedure UseUnicodeOutput(var AOutput: Text; const AIsConsole: TConsoleProbe;
  const AWriter: TConsoleUTF16Writer);

{ The number of leading bytes of ABytes that end on a character boundary:
  ALength, less a UTF-8 sequence at the end that more bytes would complete. }
function CompleteUTF8Length(const ABytes: PAnsiChar;
  const ALength: SizeInt): SizeInt;

implementation

type
  PConsoleUTF16Writer = ^TConsoleUTF16Writer;

const
  UTF8_MAX_SEQUENCE_LENGTH = 4;

function CompleteUTF8Length(const ABytes: PAnsiChar;
  const ALength: SizeInt): SizeInt;
var
  Start, SequenceLength: SizeInt;
  Lead: Byte;
begin
  Result := ALength;
  Start := ALength - 1;
  while (Start >= 0) and (Start > ALength - UTF8_MAX_SEQUENCE_LENGTH) and
    ((Ord(ABytes[Start]) and $C0) = $80) do
    Dec(Start);
  if Start < 0 then
    Exit;
  Lead := Ord(ABytes[Start]);
  if (Lead and $F8) = $F0 then
    SequenceLength := 4
  else if (Lead and $F0) = $E0 then
    SequenceLength := 3
  else if (Lead and $E0) = $C0 then
    SequenceLength := 2
  else
    Exit;
  if ALength - Start < SequenceLength then
    Result := Start;
end;

{ The write and flush function of a console text file. }
procedure WriteConsoleBuffer(var ATextRec: TextRec);
var
  Complete, Rest: SizeInt;
  Characters: UnicodeString;
  Writer: TConsoleUTF16Writer;
begin
  if ATextRec.BufPos = 0 then
    Exit;
  Complete := CompleteUTF8Length(PAnsiChar(ATextRec.BufPtr), ATextRec.BufPos);
  if Complete > 0 then
  begin
    Characters := '';
    WideStringManager.Ansi2UnicodeMoveProc(PAnsiChar(ATextRec.BufPtr),
      CP_UTF8, Characters, Complete);
    Writer := PConsoleUTF16Writer(@ATextRec.UserData)^;
    if not Writer(ATextRec.Handle, PUnicodeChar(Characters),
      Length(Characters)) then
      InOutRes := 101;
  end;
  Rest := ATextRec.BufPos - Complete;
  if Rest > 0 then
    Move(ATextRec.BufPtr^[Complete], ATextRec.BufPtr^[0], Rest);
  ATextRec.BufPos := Rest;
end;

procedure UseUnicodeOutput(var AOutput: Text; const AIsConsole: TConsoleProbe;
  const AWriter: TConsoleUTF16Writer);
begin
  if TextRec(AOutput).Mode <> fmOutput then
    Exit;
  SetTextCodePage(AOutput, CP_UTF8);
  if not AIsConsole(TextRec(AOutput).Handle) then
    Exit;
  PConsoleUTF16Writer(@TextRec(AOutput).UserData)^ := AWriter;
  TextRec(AOutput).InOutFunc := @WriteConsoleBuffer;
  TextRec(AOutput).FlushFunc := @WriteConsoleBuffer;
end;

end.
