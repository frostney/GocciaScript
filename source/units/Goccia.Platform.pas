unit Goccia.Platform;

{$I Goccia.inc}

interface

function GetBuildOS: string;
function GetBuildArch: string;
function IsGCStressBuild: Boolean;

implementation

const
  {$IF DEFINED(DARWIN)}
  BUILD_OS = 'darwin';
  {$ELSEIF DEFINED(ANDROID)}
  BUILD_OS = 'android';
  {$ELSEIF DEFINED(LINUX)}
  BUILD_OS = 'linux';
  {$ELSEIF DEFINED(MSWINDOWS)}
  BUILD_OS = 'windows';
  {$ELSEIF DEFINED(FREEBSD)}
  BUILD_OS = 'freebsd';
  {$ELSEIF DEFINED(NETBSD)}
  BUILD_OS = 'netbsd';
  {$ELSEIF DEFINED(OPENBSD)}
  BUILD_OS = 'openbsd';
  {$ELSEIF DEFINED(AIX)}
  BUILD_OS = 'aix';
  {$ELSEIF DEFINED(SOLARIS)}
  BUILD_OS = 'solaris';
  {$ELSE}
  BUILD_OS = 'unknown';
  {$ENDIF}

  {$IF defined(GOCCIA_CPU_X86_64)}
  BUILD_ARCH = 'x86_64';
  {$ELSEIF defined(GOCCIA_CPU_AARCH64)}
  BUILD_ARCH = 'aarch64';
  {$ELSEIF defined(GOCCIA_CPU_X86)}
  BUILD_ARCH = 'x86';
  {$ELSEIF DEFINED(CPUARM)}
  BUILD_ARCH = 'arm';
  {$ELSEIF DEFINED(CPUPOWERPC64)}
  BUILD_ARCH = 'powerpc64';
  {$ELSEIF DEFINED(CPUPOWERPC)}
  BUILD_ARCH = 'powerpc';
  {$ELSE}
  BUILD_ARCH = 'unknown';
  {$ENDIF}

function GetBuildOS: string;
begin
  Result := BUILD_OS;
end;

function GetBuildArch: string;
begin
  Result := BUILD_ARCH;
end;

{ True in a GC-stress build (./build.pas --gc-stress), which collects every
  few instructions. Tests read it as Goccia.build.gcStress to skip cases whose
  cost grows with the live heap times the instruction count. }
function IsGCStressBuild: Boolean;
begin
  {$IFDEF GOCCIA_GC_STRESS}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

end.
