unit Goccia.Sandbox.FileSystemErrorValues;

{ A sandbox filesystem error (Goccia.Sandbox.FileSystemErrors) as the old
  engine's Error object, with Node's code, errno, path, syscall and dest. }

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
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Sandbox.FileSystemErrors,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Primitives;

function CreateSandboxFileSystemErrorInternal(
  const AException: ESandboxFsError; const AOperation, APath,
  ADestination: string; const AHasDestination: Boolean): TGocciaObjectValue;
var
  Definition: TSandboxFileSystemErrorDefinition;
begin
  Definition := SandboxFileSystemErrorDefinition(AException);
  Result := CreateErrorObject(ERROR_NAME,
    SandboxFileSystemErrorMessage(Definition, AOperation, APath, ADestination,
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
