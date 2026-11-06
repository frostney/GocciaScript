{ Unicode text on standard output and standard error.

  FPC 3.2's RTL writes a UnicodeString to a text file by converting it to the
  file's code page. A Windows console starts in the console's output code page
  (437 or 850, for example), in which a character outside that page becomes
  '?'. Only a console accepts UTF-16 through WriteConsoleW, and a file or a
  pipe wants the UTF-8 bytes the other platforms write.

  UseUnicodeOutput makes a standard text file write UTF-8, and when its handle
  is a console, it also replaces the file's write function: the RTL fills the
  buffer with UTF-8, and each flush hands the buffer's complete characters to
  the console writer, which writes them to the console as UTF-16. When a long
  string fills the buffer in the middle of a character, the start of that
  character stays in the buffer until the rest of it arrives. Every Write and
  WriteLn to the file goes through it, so nothing that writes to standard
  output changes. The console's code page is not changed.

  The probe and the writer are parameters so that the logic runs on every
  platform under test; ProcessEncoding passes GetConsoleMode and a writer
  around WriteConsoleW on Windows. }

unit ConsoleOutput;

{$I Shared.inc}

interface

const
  { The IOResult of a failed write, as the RTL reports it for a file. }
  WRITE_FAILED_IO_RESULT = 101;

type
  { True when AHandle is a console. }
  TConsoleProbe = function(const AHandle: THandle): Boolean;
  { Writes ALength bytes of UTF-8 to the console AHandle; False on failure.
    The bytes end on a character boundary unless they end the output. }
  TConsoleWriter = function(const AHandle: THandle; const AText: PAnsiChar;
    const ALength: SizeInt): Boolean;

{ Makes AOutput, an open standard text file, write UTF-8: to a console
  through AWriter, to anything else as UTF-8 bytes. }
procedure UseUnicodeOutput(var AOutput: Text; const AIsConsole: TConsoleProbe;
  const AWriter: TConsoleWriter);

{ The number of leading bytes of ABytes that end on a character boundary:
  ALength, less a UTF-8 sequence at the end that more bytes would complete. }
function CompleteUTF8Length(const ABytes: PAnsiChar;
  const ALength: SizeInt): SizeInt;

implementation

type
  PConsoleWriter = ^TConsoleWriter;

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

{ The write and flush function of a console text file. The RTL calls it with
  a full buffer in the middle of a write, the only time a character can be
  split; any other call, from WriteLn, Flush, Close or the flush at exit,
  writes everything, so no byte is left behind when the output ends. }
procedure WriteConsoleBuffer(var ATextRec: TextRec);
var
  Complete, Rest: SizeInt;
  Writer: TConsoleWriter;
begin
  if ATextRec.BufPos = 0 then
    Exit;
  if ATextRec.BufPos < ATextRec.BufSize then
    Complete := ATextRec.BufPos
  else
    Complete := CompleteUTF8Length(PAnsiChar(ATextRec.BufPtr),
      ATextRec.BufPos);
  if Complete > 0 then
  begin
    Writer := PConsoleWriter(@ATextRec.UserData)^;
    if not Writer(ATextRec.Handle, PAnsiChar(ATextRec.BufPtr), Complete) then
      InOutRes := WRITE_FAILED_IO_RESULT;
  end;
  Rest := ATextRec.BufPos - Complete;
  if Rest > 0 then
    Move(ATextRec.BufPtr^[Complete], ATextRec.BufPtr^[0], Rest);
  ATextRec.BufPos := Rest;
end;

procedure UseUnicodeOutput(var AOutput: Text; const AIsConsole: TConsoleProbe;
  const AWriter: TConsoleWriter);
begin
  if TextRec(AOutput).Mode <> fmOutput then
    Exit;
  SetTextCodePage(AOutput, CP_UTF8);
  if not AIsConsole(TextRec(AOutput).Handle) then
    Exit;
  PConsoleWriter(@TextRec(AOutput).UserData)^ := AWriter;
  TextRec(AOutput).InOutFunc := @WriteConsoleBuffer;
  TextRec(AOutput).FlushFunc := @WriteConsoleBuffer;
end;

end.
