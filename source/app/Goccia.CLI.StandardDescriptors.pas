unit Goccia.CLI.StandardDescriptors;

{$I Goccia.inc}

{ A CLI started with standard input, output or error closed would hand that
  descriptor number to the next file it opens: with descriptor 2 closed, the
  --log file became descriptor 2 and took every line console.error wrote to
  standard error. On a Unix, this unit's initialization opens /dev/null onto
  each of descriptors 0, 1 and 2 that is closed, before any file is opened, as
  Node does at startup. Writes to a descriptor that was closed then go
  nowhere. The TGocciaCLIApplication hosts (GocciaRunner, GocciaTestRunner,
  GocciaBenchmarkRunner, GocciaREPL and GocciaBundler) use it through
  Goccia.CLI.Application; the bare loader, the harnesses and the engine do
  not, so an embedder's descriptors are its own. }

interface

implementation

{$IFDEF UNIX}
uses
  BaseUnix;

procedure OpenClosedStandardDescriptors;
var
  Descriptor, Opened: cint;
begin
  for Descriptor := 0 to 2 do
    if (FpFcntl(Descriptor, F_GETFD) = -1) and (FpGetErrno = ESysEBADF) then
    begin
      { open returns the lowest free descriptor, which is this one: the lower
        ones are open, or were opened by an earlier iteration. }
      Opened := FpOpen('/dev/null', O_RDWR);
      if (Opened >= 0) and (Opened <> Descriptor) then
        FpClose(Opened);
    end;
end;

initialization
  OpenClosedStandardDescriptors;
{$ENDIF}

end.
