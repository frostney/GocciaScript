program Goccia.Sandbox.FileSystemErrors.Test;

{$I Goccia.inc}

uses
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF}
  SysUtils,

  SandboxVirtualFileSystem,
  TestingPascalLibrary,

  Goccia.Constants.PropertyNames,
  Goccia.Sandbox.FileSystemErrors,
  Goccia.TestSetup,
  Goccia.Values.ObjectValue;

type
  TSandboxFileSystemErrorTests = class(TTestSuite)
  private
    procedure AssertMapping(const AException: ESandboxFsError;
      const ACode: string; const AErrno: Integer;
      const ADescription: string);
    procedure TestMapsTypedExceptions;
    procedure TestCreatesSystemErrorProperties;
    procedure TestAddsDestinationForTwoPathOperation;
    procedure TestOmitsDestinationForOnePathOperation;
    procedure TestMapsUnknownBaseErrorToInputOutputError;
  public
    procedure SetupTests; override;
  end;

const
{$IFDEF MSWINDOWS}
  EXPECTED_ERRNO_EBUSY = -4082;
  EXPECTED_ERRNO_EEXIST = -4075;
  EXPECTED_ERRNO_EINVAL = -4071;
  EXPECTED_ERRNO_EIO = -4070;
  EXPECTED_ERRNO_EISDIR = -4068;
  EXPECTED_ERRNO_ENOENT = -4058;
  EXPECTED_ERRNO_ENOSPC = -4055;
  EXPECTED_ERRNO_ENOTDIR = -4052;
  EXPECTED_ERRNO_ENOTEMPTY = -4051;
{$ELSE}
  EXPECTED_ERRNO_EBUSY = -ESysEBUSY;
  EXPECTED_ERRNO_EEXIST = -ESysEEXIST;
  EXPECTED_ERRNO_EINVAL = -ESysEINVAL;
  EXPECTED_ERRNO_EIO = -ESysEIO;
  EXPECTED_ERRNO_EISDIR = -ESysEISDIR;
  EXPECTED_ERRNO_ENOENT = -ESysENOENT;
  EXPECTED_ERRNO_ENOSPC = -ESysENOSPC;
  EXPECTED_ERRNO_ENOTDIR = -ESysENOTDIR;
  EXPECTED_ERRNO_ENOTEMPTY = -ESysENOTEMPTY;
{$ENDIF}

procedure TSandboxFileSystemErrorTests.SetupTests;
begin
  Test('Maps every typed sandbox filesystem exception',
    TestMapsTypedExceptions);
  Test('Creates Node-shaped system error properties',
    TestCreatesSystemErrorProperties);
  Test('Adds destination for two-path operations',
    TestAddsDestinationForTwoPathOperation);
  Test('Omits destination for one-path operations',
    TestOmitsDestinationForOnePathOperation);
  Test('Maps unknown base errors to EIO',
    TestMapsUnknownBaseErrorToInputOutputError);
end;

procedure TSandboxFileSystemErrorTests.AssertMapping(
  const AException: ESandboxFsError; const ACode: string;
  const AErrno: Integer; const ADescription: string);
var
  ErrorObject: TGocciaObjectValue;
begin
  try
    ErrorObject := CreateSandboxFileSystemError(AException, 'readFile',
      '/input.txt');
    try
      Expect<string>(ErrorObject.GetProperty(PROP_CODE).ToStringLiteral.Value)
        .ToBe(ACode);
      Expect<Double>(ErrorObject.GetProperty(PROP_ERRNO).ToNumberLiteral.Value)
        .ToBe(AErrno);
      Expect<string>(ErrorObject.GetProperty(PROP_MESSAGE).ToStringLiteral.Value)
        .ToBe(ACode + ': ' + ADescription + ', readFile ''/input.txt''');
    finally
      ErrorObject.Free;
    end;
  finally
    AException.Free;
  end;
end;

procedure TSandboxFileSystemErrorTests.TestMapsTypedExceptions;
begin
  AssertMapping(ESandboxFsInvalidPath.Create('ignored'), 'EINVAL',
    EXPECTED_ERRNO_EINVAL,
    'invalid argument');
  AssertMapping(ESandboxFsNotFound.Create('ignored'), 'ENOENT',
    EXPECTED_ERRNO_ENOENT,
    'no such file or directory');
  AssertMapping(ESandboxFsExists.Create('ignored'), 'EEXIST',
    EXPECTED_ERRNO_EEXIST,
    'file already exists');
  AssertMapping(ESandboxFsNotADirectory.Create('ignored'), 'ENOTDIR',
    EXPECTED_ERRNO_ENOTDIR,
    'not a directory');
  AssertMapping(ESandboxFsIsADirectory.Create('ignored'), 'EISDIR',
    EXPECTED_ERRNO_EISDIR,
    'illegal operation on a directory');
  AssertMapping(ESandboxFsNotEmpty.Create('ignored'), 'ENOTEMPTY',
    EXPECTED_ERRNO_ENOTEMPTY,
    'directory not empty');
  AssertMapping(ESandboxFsBusy.Create('ignored'), 'EBUSY', EXPECTED_ERRNO_EBUSY,
    'resource busy or locked');
  AssertMapping(ESandboxFsQuotaExceeded.Create('ignored'), 'ENOSPC',
    EXPECTED_ERRNO_ENOSPC,
    'no space left on device');
end;

procedure TSandboxFileSystemErrorTests.TestCreatesSystemErrorProperties;
var
  SourceException: ESandboxFsNotFound;
  ErrorObject: TGocciaObjectValue;
begin
  SourceException := ESandboxFsNotFound.Create('ignored');
  try
    ErrorObject := CreateSandboxFileSystemError(SourceException, 'stat',
      '/missing.txt');
    try
      Expect<Boolean>(ErrorObject.HasErrorData).ToBe(True);
      Expect<string>(ErrorObject.GetProperty(PROP_NAME).ToStringLiteral.Value)
        .ToBe('Error');
      Expect<string>(ErrorObject.GetProperty(PROP_CODE).ToStringLiteral.Value)
        .ToBe('ENOENT');
      Expect<Boolean>(ErrorObject.GetProperty(PROP_ERRNO).ToNumberLiteral.Value <
        0).ToBe(True);
      Expect<string>(ErrorObject.GetProperty(PROP_PATH).ToStringLiteral.Value)
        .ToBe('/missing.txt');
      Expect<string>(ErrorObject.GetProperty(PROP_SYSCALL).ToStringLiteral.Value)
        .ToBe('stat');
    finally
      ErrorObject.Free;
    end;
  finally
    SourceException.Free;
  end;
end;

procedure TSandboxFileSystemErrorTests.TestAddsDestinationForTwoPathOperation;
var
  SourceException: ESandboxFsNotFound;
  ErrorObject: TGocciaObjectValue;
begin
  SourceException := ESandboxFsNotFound.Create('ignored');
  try
    ErrorObject := CreateSandboxFileSystemError(SourceException, 'rename',
      '/source.txt', '/destination.txt');
    try
      Expect<string>(ErrorObject.GetProperty(PROP_DEST).ToStringLiteral.Value)
        .ToBe('/destination.txt');
      Expect<string>(ErrorObject.GetProperty(PROP_MESSAGE).ToStringLiteral.Value)
        .ToBe('ENOENT: no such file or directory, rename ''/source.txt'' -> ' +
          '''/destination.txt''');
    finally
      ErrorObject.Free;
    end;

    ErrorObject := CreateSandboxFileSystemError(SourceException, 'rename',
      '/source.txt', '');
    try
      Expect<Boolean>(ErrorObject.HasOwnProperty(PROP_DEST)).ToBe(True);
      Expect<string>(ErrorObject.GetProperty(PROP_DEST).ToStringLiteral.Value)
        .ToBe('');
      Expect<string>(ErrorObject.GetProperty(PROP_MESSAGE).ToStringLiteral.Value)
        .ToBe('ENOENT: no such file or directory, rename ''/source.txt'' -> ' +
          '''''');
    finally
      ErrorObject.Free;
    end;
  finally
    SourceException.Free;
  end;
end;

procedure TSandboxFileSystemErrorTests.TestOmitsDestinationForOnePathOperation;
var
  SourceException: ESandboxFsNotFound;
  ErrorObject: TGocciaObjectValue;
begin
  SourceException := ESandboxFsNotFound.Create('ignored');
  try
    ErrorObject := CreateSandboxFileSystemError(SourceException, 'readFile',
      '/input.txt');
    try
      Expect<Boolean>(ErrorObject.HasOwnProperty(PROP_DEST)).ToBe(False);
    finally
      ErrorObject.Free;
    end;
  finally
    SourceException.Free;
  end;
end;

procedure TSandboxFileSystemErrorTests.TestMapsUnknownBaseErrorToInputOutputError;
begin
  AssertMapping(ESandboxFsError.Create('ignored'), 'EIO', EXPECTED_ERRNO_EIO,
    'i/o error');
end;

begin
  TestRunnerProgram.AddSuite(
    TSandboxFileSystemErrorTests.Create('SandboxFileSystemErrors'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
