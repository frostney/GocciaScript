{ Where a console line goes when the console writes to the process itself:
  warn, error, assert and trace to standard error, as in Node, and every other
  method to standard output, each line flushed as it is logged. }

unit ConsoleStreams;

{$I Shared.inc}

interface

{ True for the console methods whose lines go to standard error. }
function ConsoleMethodWritesToStderr(const AMethod: string): Boolean;

{ Writes ALine and a line break to standard error or standard output and
  flushes it, so each line reaches a pipe or file as it is logged, not when
  the RTL's buffer fills or the process ends. Before a standard-error line,
  whatever is still in standard output's buffer goes out first, so with both
  streams on one pipe or file the lines keep the order they were logged in. A
  write that fails with an error, to a closed or read-only descriptor, a full
  disk or a pipe whose reader has gone, is dropped, as Node's console drops
  it: SigPipeGuard holds off the SIGPIPE such a pipe raises on this thread, so
  the script goes on. }
procedure WriteConsoleLine(const AToStderr: Boolean; const ALine: UnicodeString);

implementation

uses
  {$IFDEF DARWIN}BaseUnix,{$ENDIF}
  SigPipeGuard;

{$IFDEF DARWIN}
const
  { fcntl: a write to the descriptor's pipe whose reader has gone fails with EPIPE and raises no SIGPIPE }
  F_SETNOSIGPIPE = 73;
  F_GETNOSIGPIPE = 74;

{ Darwin raises a pipe write's SIGPIPE for the process, not the writing thread (XNU's dofilewrite), so a console
  line written from an engine's thread would end the process through another thread that does not hold it off.
  The descriptor's own no-SIGPIPE flag covers the write instead; True when it was set here, to be cleared after. }
function HoldOffPipeSignal(const AHandle: THandle): Boolean;
begin
  Result := (FpFcntl(cint(AHandle), F_GETNOSIGPIPE) = 0) and
    (FpFcntl(cint(AHandle), F_SETNOSIGPIPE, 1) = 0);
end;

procedure ReleasePipeSignal(const AHandle: THandle; const ASet: Boolean);
begin
  if ASet then
    FpFcntl(cint(AHandle), F_SETNOSIGPIPE, 0);
end;
{$ENDIF}

function ConsoleMethodWritesToStderr(const AMethod: string): Boolean;
begin
  Result := (AMethod = 'warn') or (AMethod = 'error') or
    (AMethod = 'assert') or (AMethod = 'trace');
end;

{ Drops what a failed write left in F's buffer: the RTL keeps the unwritten
  tail of a line whose write failed, and would write it before the next line
  or when the program ends, where nothing holds off the SIGPIPE it raises. }
procedure DiscardUnwritten(var F: Text);
begin
  TextRec(F).BufPos := 0;
end;

procedure WriteConsoleLine(const AToStderr: Boolean; const ALine: UnicodeString);
var
  Guard: TSigPipeGuard;
  {$IFDEF DARWIN}
  OutHeld, ErrHeld: Boolean;
  {$ENDIF}
begin
  {$IFDEF DARWIN}
  OutHeld := HoldOffPipeSignal(TextRec(Output).Handle);
  ErrHeld := AToStderr and HoldOffPipeSignal(TextRec(ErrOutput).Handle);
  {$ENDIF}
  BeginSigPipeGuard(Guard);
  try
    {$I-}
    if AToStderr then
    begin
      Flush(Output);
      if InOutRes <> 0 then
        DiscardUnwritten(Output);
      InOutRes := 0;
      WriteLn(ErrOutput, ALine);
      Flush(ErrOutput);
      if InOutRes <> 0 then
        DiscardUnwritten(ErrOutput);
    end
    else
    begin
      WriteLn(Output, ALine);
      Flush(Output);
      if InOutRes <> 0 then
        DiscardUnwritten(Output);
    end;
    {$I+}
    InOutRes := 0;
  finally
    { A long line goes out in several writes, and one that succeeds after a
      failed one clears the failure, so a SIGPIPE is looked for whatever the
      last write's status. }
    EndSigPipeGuard(Guard);
    {$IFDEF DARWIN}
    ReleasePipeSignal(TextRec(Output).Handle, OutHeld);
    if AToStderr then
      ReleasePipeSignal(TextRec(ErrOutput).Handle, ErrHeld);
    {$ENDIF}
  end;
end;

end.
