unit Goccia.Sandbox.FileSystemErrors;

{$I Goccia.inc}

interface

uses
  SandboxVirtualFileSystem,

  Goccia.Values.ObjectValue;

function CreateSandboxFileSystemError(const AException: ESandboxFsError;
  const AOperation, APath: string): TGocciaObjectValue; overload;
function CreateSandboxFileSystemError(const AException: ESandboxFsError;
  const AOperation, APath, ADestination: string): TGocciaObjectValue; overload;

implementation

uses
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF}
  SysUtils,

  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Primitives;

const
{$IFDEF WINDOWS}
  ERRNO_EBUSY = -4082;
  ERRNO_EEXIST = -4075;
  ERRNO_EINVAL = -4071;
  ERRNO_EIO = -4070;
  ERRNO_EISDIR = -4068;
  ERRNO_ENOENT = -4058;
  ERRNO_ENOSPC = -4055;
  ERRNO_ENOTDIR = -4052;
  ERRNO_ENOTEMPTY = -4051;
{$ELSE}
  ERRNO_EBUSY = -ESysEBUSY;
  ERRNO_EEXIST = -ESysEEXIST;
  ERRNO_EINVAL = -ESysEINVAL;
  ERRNO_EIO = -ESysEIO;
  ERRNO_EISDIR = -ESysEISDIR;
  ERRNO_ENOENT = -ESysENOENT;
  ERRNO_ENOSPC = -ESysENOSPC;
  ERRNO_ENOTDIR = -ESysENOTDIR;
  ERRNO_ENOTEMPTY = -ESysENOTEMPTY;
{$ENDIF}

type
  TSandboxFileSystemErrorDefinition = record
    Code: string;
    Errno: Integer;
    Description: string;
  end;

function ErrorDefinition(const AException: ESandboxFsError):
  TSandboxFileSystemErrorDefinition;
begin
  if AException is ESandboxFsInvalidPath then
  begin
    Result.Code := 'EINVAL';
    Result.Errno := ERRNO_EINVAL;
    Result.Description := 'invalid argument';
  end
  else if AException is ESandboxFsNotFound then
  begin
    Result.Code := 'ENOENT';
    Result.Errno := ERRNO_ENOENT;
    Result.Description := 'no such file or directory';
  end
  else if AException is ESandboxFsExists then
  begin
    Result.Code := 'EEXIST';
    Result.Errno := ERRNO_EEXIST;
    Result.Description := 'file already exists';
  end
  else if AException is ESandboxFsNotADirectory then
  begin
    Result.Code := 'ENOTDIR';
    Result.Errno := ERRNO_ENOTDIR;
    Result.Description := 'not a directory';
  end
  else if AException is ESandboxFsIsADirectory then
  begin
    Result.Code := 'EISDIR';
    Result.Errno := ERRNO_EISDIR;
    Result.Description := 'illegal operation on a directory';
  end
  else if AException is ESandboxFsNotEmpty then
  begin
    Result.Code := 'ENOTEMPTY';
    Result.Errno := ERRNO_ENOTEMPTY;
    Result.Description := 'directory not empty';
  end
  else if AException is ESandboxFsBusy then
  begin
    Result.Code := 'EBUSY';
    Result.Errno := ERRNO_EBUSY;
    Result.Description := 'resource busy or locked';
  end
  else if AException is ESandboxFsQuotaExceeded then
  begin
    Result.Code := 'ENOSPC';
    Result.Errno := ERRNO_ENOSPC;
    Result.Description := 'no space left on device';
  end
  else
  begin
    Result.Code := 'EIO';
    Result.Errno := ERRNO_EIO;
    Result.Description := 'i/o error';
  end;
end;

function ErrorMessage(const ADefinition: TSandboxFileSystemErrorDefinition;
  const AOperation, APath, ADestination: string;
  const AHasDestination: Boolean): string;
begin
  Result := Format('%s: %s, %s ''%s''', [ADefinition.Code,
    ADefinition.Description, AOperation, APath]);
  if AHasDestination then
    Result := Result + Format(' -> ''%s''', [ADestination]);
end;

function CreateSandboxFileSystemErrorInternal(
  const AException: ESandboxFsError; const AOperation, APath,
  ADestination: string; const AHasDestination: Boolean): TGocciaObjectValue;
var
  Definition: TSandboxFileSystemErrorDefinition;
begin
  Definition := ErrorDefinition(AException);
  Result := CreateErrorObject(ERROR_NAME,
    ErrorMessage(Definition, AOperation, APath, ADestination,
      AHasDestination));
  Result.AssignProperty(PROP_CODE,
    TGocciaStringLiteralValue.Create(Definition.Code));
  Result.AssignProperty(PROP_ERRNO,
    TGocciaNumberLiteralValue.Create(Definition.Errno));
  Result.AssignProperty(PROP_PATH, TGocciaStringLiteralValue.Create(APath));
  Result.AssignProperty(PROP_SYSCALL,
    TGocciaStringLiteralValue.Create(AOperation));
  if AHasDestination then
    Result.AssignProperty(PROP_DEST,
      TGocciaStringLiteralValue.Create(ADestination));
end;

function CreateSandboxFileSystemError(const AException: ESandboxFsError;
  const AOperation, APath: string): TGocciaObjectValue;
begin
  Result := CreateSandboxFileSystemErrorInternal(AException, AOperation,
    APath, '', False);
end;

function CreateSandboxFileSystemError(const AException: ESandboxFsError;
  const AOperation, APath, ADestination: string): TGocciaObjectValue;
begin
  Result := CreateSandboxFileSystemErrorInternal(AException, AOperation,
    APath, ADestination, True);
end;

end.
