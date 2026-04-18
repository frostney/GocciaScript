unit Goccia.Platform;

{$I Goccia.inc}

interface

function GetBuildOS: string;
function GetBuildArch: string;

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

  {$IF DEFINED(CPUX86_64)}
  BUILD_ARCH = 'x86_64';
  {$ELSEIF DEFINED(CPUAARCH64)}
  BUILD_ARCH = 'aarch64';
  {$ELSEIF DEFINED(CPUI386)}
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

end.
