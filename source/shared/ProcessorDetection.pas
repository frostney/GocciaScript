unit ProcessorDetection;

{$I Shared.inc}

interface

// Returns the online processor count, with a minimum of one.
function GetProcessorCount: Integer;

implementation

uses
  Classes;

{ FPC 3.2.2 can report one processor on multicore macOS hosts. Use
  the native online processor count for both CLI and compliance workers. }

{$IFDEF UNIX}
// C long is pointer-sized on the supported UNIX targets.
function libc_sysconf(Name: Integer): NativeInt; cdecl; external 'c' name 'sysconf';
{$ENDIF}

function GetProcessorCount: Integer;
{$IFDEF UNIX}
const
  {$IFDEF DARWIN}
  SC_NPROCESSORS_ONLN = 58;
  {$ELSE}
  SC_NPROCESSORS_ONLN = 84;   { Linux }
  {$ENDIF}
var
  N: NativeInt;
{$ENDIF}
begin
  {$IFDEF UNIX}
  N := libc_sysconf(SC_NPROCESSORS_ONLN);
  if N > 0 then
    Result := Integer(N)
  else
    Result := 1;
  {$ELSE}
  Result := TThread.ProcessorCount;
  if Result < 1 then
    Result := 1;
  {$ENDIF}
end;

end.
