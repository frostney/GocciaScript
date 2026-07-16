unit Goccia.RuntimeExtensions.Sandbox;

{$I Goccia.inc}

interface

uses
  SysUtils,

  SandboxVirtualFileSystem,

  Goccia.Arguments.Collection,
  Goccia.Modules,
  Goccia.Runtime,
  Goccia.Sandbox.Context,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaSandboxRuntimeExtension = class(TGocciaRuntimeExtension)
  private
    FContext: TGocciaSandboxContext;
    FFsModule: TGocciaModule;
    FGocciaModule: TGocciaModule;

    function EnsureStatsPrototype: TGocciaObjectValue;
    function CreateStatsValue(const AStat: TSandboxFsStat): TGocciaValue;
    function CreateStatsDate(const AMilliseconds: Double): TGocciaValue;
    function StatsAtime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StatsMtime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StatsCtime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StatsBirthtime(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StatsIsFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StatsIsDirectory(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function StatsIsSymbolicLink(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function ReadFileValue(const APath: string;
      const ATextResult: Boolean): TGocciaValue;
    function WriteFileValue(const APath: string;
      const AData: TBytes): TGocciaValue;
    function AppendFileValue(const APath: string;
      const AData: TBytes): TGocciaValue;
    function MkdirValue(const APath: string; const ARecursive,
      AReturnFirstCreatedPath: Boolean): TGocciaValue;
    function ReaddirValue(const APath: string): TGocciaValue;
    function StatValue(const APath: string): TGocciaValue;
    function ExistsValue(const APath: string): TGocciaValue;
    function RmValue(const APath: string;
      const ARecursive: Boolean): TGocciaValue;
    function RenameValue(const APath,
      ADestination: string): TGocciaValue;
    function CopyFileValue(const APath,
      ADestination: string): TGocciaValue;

    function SandboxDollar(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function RunScript(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function FsReadFileSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsWriteFileSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsAppendFileSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsMkdirSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsReaddirSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsStatSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsExistsSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsRmSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsRenameSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsCopyFileSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsReadFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsWriteFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsAppendFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsMkdir(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsReaddir(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsStat(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsExists(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsRm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsRename(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsCopyFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function FsPromisesReadFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesWriteFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesAppendFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesMkdir(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesReaddir(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesStat(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesRm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesRename(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesCopyFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    function CreateFsNamespace: TGocciaObjectValue;
    function CreateGocciaNamespace: TGocciaObjectValue;
  public
    constructor Create(const AContext: TGocciaSandboxContext);
    procedure Attach(const ARuntime: TGocciaRuntimeCore); override;
    procedure Detach; override;
  end;

implementation

uses
  base64,
  SandboxShell,

  Goccia.Constants.ErrorNames,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.MicrotaskQueue,
  Goccia.Realm,
  Goccia.Sandbox.FileSystemErrors,
  Goccia.Shims,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.TypedArrayValue;

type
  TGocciaSandboxFsOperation = (
    sfoReadFile,
    sfoWriteFile,
    sfoAppendFile,
    sfoMkdir,
    sfoReaddir,
    sfoStat,
    sfoExists,
    sfoRm,
    sfoRename,
    sfoCopyFile
  );

  TGocciaSandboxFsCompletion = (
    sfcErrorOnly,
    sfcValue,
    sfcMkdir,
    sfcExists
  );

  TGocciaSandboxFsJob = class(TGocciaMicrotaskJob)
  private
    FExtension: TGocciaSandboxRuntimeExtension;
    FOperation: TGocciaSandboxFsOperation;
    FPath: string;
    FDestination: string;
    FData: TBytes;
    FTextResult: Boolean;
    FRecursive: Boolean;
    FCompletion: TGocciaSandboxFsCompletion;
    FCallback: TGocciaValue;
    FPromise: TGocciaPromiseValue;
    procedure CompleteFailure(const AError: TGocciaValue);
    procedure CompleteSuccess(const AValue: TGocciaValue);
  public
    constructor Create(const AExtension: TGocciaSandboxRuntimeExtension;
      const AOperation: TGocciaSandboxFsOperation; const APath,
      ADestination: string; const AData: TBytes; const ATextResult,
      ARecursive: Boolean;
      const ACompletion: TGocciaSandboxFsCompletion;
      const ACallback: TGocciaValue; const APromise: TGocciaPromiseValue);
    procedure Execute; override;
    procedure CaptureRoots(
      const AContainer: TGocciaObjectValue); override;
  end;

  TGocciaSandboxStatValue = class(TGocciaObjectValue)
  private
    FKind: TSandboxFsNodeKind;
    FAtimeMs: Double;
    FMtimeMs: Double;
    FCtimeMs: Double;
    FBirthtimeMs: Double;
  public
    constructor Create(const APrototype: TGocciaObjectValue;
      const AStat: TSandboxFsStat);
  end;

  TGocciaSandboxShellCommandValue = class(TGocciaObjectValue)
  private
    FContext: TGocciaSandboxContext;
    FCommandLine: string;
    FNoThrow: Boolean;
    FQuiet: Boolean;
    FExecuted: Boolean;
    FResult: TExecResult;
    procedure EnsureExecuted;
    function ResultObject: TGocciaObjectValue;
    function Fulfilled(const AValue: TGocciaValue): TGocciaPromiseValue;
  public
    constructor Create(const AContext: TGocciaSandboxContext;
      const ACommandLine: string);
    function Run(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Text(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Json(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function Quiet(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function NoThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

function AddNativeFunction(const AObject: TGocciaObjectValue;
  const AName: string; const ACallback: TGocciaNativeFunctionCallback;
  const AArity: Integer): TGocciaNativeFunctionValue;
begin
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(ACallback,
    AName, AArity);
  AObject.SetProperty(AName, Result);
end;

var
  GSandboxStatsPrototypeSlot: TGocciaRealmSlotId;

function FulfilledPromise(const AValue: TGocciaValue): TGocciaPromiseValue;
begin
  Result := TGocciaPromiseValue.Create;
  Result.Resolve(AValue);
end;

function RejectedPromise(const AValue: TGocciaValue): TGocciaPromiseValue;
begin
  Result := TGocciaPromiseValue.Create;
  Result.Reject(AValue);
end;

{ TGocciaSandboxFsJob }

constructor TGocciaSandboxFsJob.Create(
  const AExtension: TGocciaSandboxRuntimeExtension;
  const AOperation: TGocciaSandboxFsOperation; const APath,
  ADestination: string; const AData: TBytes; const ATextResult,
  ARecursive: Boolean;
  const ACompletion: TGocciaSandboxFsCompletion;
  const ACallback: TGocciaValue; const APromise: TGocciaPromiseValue);
begin
  inherited Create;
  FExtension := AExtension;
  FOperation := AOperation;
  FPath := APath;
  FDestination := ADestination;
  FData := Copy(AData);
  FTextResult := ATextResult;
  FRecursive := ARecursive;
  FCompletion := ACompletion;
  FCallback := ACallback;
  FPromise := APromise;
end;

procedure TGocciaSandboxFsJob.CompleteFailure(const AError: TGocciaValue);
var
  CallbackArgs: TGocciaArgumentsCollection;
  ErrorRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(ErrorRoot);
  try
    AddTempRootIfNeeded(ErrorRoot, AError);
    if Assigned(FPromise) then
    begin
      FPromise.Reject(AError);
      Exit;
    end;

    if FCompletion = sfcExists then
      CallbackArgs := TGocciaArgumentsCollection.Create([
        TGocciaBooleanLiteralValue.FalseValue])
    else
      CallbackArgs := TGocciaArgumentsCollection.Create([AError]);
    try
      InvokeCallable(FCallback, CallbackArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallbackArgs.Free;
    end;
  finally
    RemoveTempRootIfNeeded(ErrorRoot);
  end;
end;

procedure TGocciaSandboxFsJob.CompleteSuccess(const AValue: TGocciaValue);
var
  CallbackArgs: TGocciaArgumentsCollection;
  ValueRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(ValueRoot);
  try
    AddTempRootIfNeeded(ValueRoot, AValue);
    if Assigned(FPromise) then
    begin
      FPromise.Resolve(AValue);
      Exit;
    end;

    case FCompletion of
      sfcErrorOnly:
        CallbackArgs := TGocciaArgumentsCollection.Create([
          TGocciaNullLiteralValue.NullValue]);
      sfcValue:
        CallbackArgs := TGocciaArgumentsCollection.Create([
          TGocciaNullLiteralValue.NullValue, AValue]);
      sfcMkdir:
        if FRecursive then
          CallbackArgs := TGocciaArgumentsCollection.Create([
            TGocciaNullLiteralValue.NullValue, AValue])
        else
          CallbackArgs := TGocciaArgumentsCollection.Create([
            TGocciaNullLiteralValue.NullValue]);
      sfcExists:
        CallbackArgs := TGocciaArgumentsCollection.Create([AValue]);
    else
      CallbackArgs := TGocciaArgumentsCollection.Create([
        TGocciaNullLiteralValue.NullValue]);
    end;
    try
      InvokeCallable(FCallback, CallbackArgs,
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallbackArgs.Free;
    end;
  finally
    RemoveTempRootIfNeeded(ValueRoot);
  end;
end;

procedure TGocciaSandboxFsJob.Execute;
var
  Value: TGocciaValue;
begin
  try
    case FOperation of
      sfoReadFile:
        Value := FExtension.ReadFileValue(FPath, FTextResult);
      sfoWriteFile:
        Value := FExtension.WriteFileValue(FPath, FData);
      sfoAppendFile:
        Value := FExtension.AppendFileValue(FPath, FData);
      sfoMkdir:
        Value := FExtension.MkdirValue(FPath, FRecursive, True);
      sfoReaddir:
        Value := FExtension.ReaddirValue(FPath);
      sfoStat:
        Value := FExtension.StatValue(FPath);
      sfoExists:
        Value := FExtension.ExistsValue(FPath);
      sfoRm:
        Value := FExtension.RmValue(FPath, FRecursive);
      sfoRename:
        Value := FExtension.RenameValue(FPath, FDestination);
      sfoCopyFile:
        Value := FExtension.CopyFileValue(FPath, FDestination);
    else
      raise Exception.Create('Unsupported filesystem operation');
    end;
  except
    on E: TGocciaThrowValue do
    begin
      CompleteFailure(E.Value);
      Exit;
    end;
    on E: Exception do
    begin
      CompleteFailure(CreateErrorObject(ERROR_NAME, E.Message));
      Exit;
    end;
  end;
  CompleteSuccess(Value);
end;

procedure TGocciaSandboxFsJob.CaptureRoots(
  const AContainer: TGocciaObjectValue);
begin
  if Assigned(FCallback) then
    AContainer.SetProperty('callback', FCallback);
  if Assigned(FPromise) then
    AContainer.SetProperty('promise', FPromise);
end;

function RequireStringArg(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer; const AMethod: string): string;
var
  Value: TGocciaValue;
begin
  Value := AArgs.GetElement(AIndex);
  if (Value is TGocciaUndefinedLiteralValue) or
     (Value is TGocciaNullLiteralValue) then
    ThrowTypeError(AMethod + ' requires a path string');
  Result := Value.ToStringLiteral.Value;
end;

function OptionRecursive(const AValue: TGocciaValue): Boolean;
var
  RecursiveValue: TGocciaValue;
begin
  if AValue is TGocciaBooleanLiteralValue then
    Exit(TGocciaBooleanLiteralValue(AValue).Value);
  if AValue is TGocciaObjectValue then
  begin
    RecursiveValue := TGocciaObjectValue(AValue).GetProperty('recursive');
    Result := (RecursiveValue is TGocciaBooleanLiteralValue) and
      TGocciaBooleanLiteralValue(RecursiveValue).Value;
    Exit;
  end;
  Result := False;
end;

function WantsTextResult(const AValue: TGocciaValue): Boolean;
var
  EncodingValue: TGocciaValue;
  Encoding: string;
begin
  if AValue is TGocciaStringLiteralValue then
  begin
    Encoding := LowerCase(TGocciaStringLiteralValue(AValue).Value);
    Exit((Encoding = 'utf8') or (Encoding = 'utf-8'));
  end;
  if AValue is TGocciaObjectValue then
  begin
    EncodingValue := TGocciaObjectValue(AValue).GetProperty('encoding');
    if EncodingValue is TGocciaStringLiteralValue then
    begin
      Encoding := LowerCase(TGocciaStringLiteralValue(EncodingValue).Value);
      Exit((Encoding = 'utf8') or (Encoding = 'utf-8'));
    end;
  end;
  Result := False;
end;

function BytesToUint8Array(const ABytes: TBytes): TGocciaTypedArrayValue;
var
  Data: TBytes;
  I: Integer;
begin
  Result := TGocciaTypedArrayValue.Create(takUint8, Length(ABytes));
  Data := Result.BufferData;
  for I := 0 to High(ABytes) do
    Data[Result.ByteOffset + I] := ABytes[I];
end;

function ValueToBytes(const AValue: TGocciaValue; const AMethod: string): TBytes;
var
  Text: string;
  TA: TGocciaTypedArrayValue;
  Data: TBytes;
  I: Integer;
begin
  if AValue is TGocciaStringLiteralValue then
  begin
    Text := TGocciaStringLiteralValue(AValue).Value;
    SetLength(Result, Length(Text));
    if Length(Text) > 0 then
      Move(Text[1], Result[0], Length(Text));
    Exit;
  end;

  if AValue is TGocciaTypedArrayValue then
  begin
    TA := TGocciaTypedArrayValue(AValue);
    Data := TA.BufferData;
    SetLength(Result, TA.Length);
    for I := 0 to TA.Length - 1 do
      Result[I] := Data[TA.ByteOffset + I];
    Exit;
  end;

  ThrowTypeError(AMethod + ' requires string or Uint8Array data');
  Result := nil;
end;

function IsMissingValue(const AValue: TGocciaValue): Boolean;
begin
  Result := (not Assigned(AValue)) or
    (AValue is TGocciaUndefinedLiteralValue) or
    (AValue is TGocciaNullLiteralValue);
end;

function RequireAsyncPath(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer; const AMethod: string): string;
var
  Value: TGocciaValue;
begin
  Value := AArgs.GetElement(AIndex);
  if not (Value is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' requires a path string');
  Result := TGocciaStringLiteralValue(Value).Value;
end;

function RequireAsyncData(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer; const AMethod: string): TBytes;
begin
  Result := ValueToBytes(AArgs.GetElement(AIndex), AMethod);
end;

function RequireFsCallback(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer; const AMethod: string): TGocciaValue;
begin
  Result := AArgs.GetElement(AIndex);
  if not Assigned(Result) or not Result.IsCallable then
    ThrowTypeError(AMethod + ' requires a callback function');
end;

procedure ValidateArgumentCount(const AArgs: TGocciaArgumentsCollection;
  const AMaximum: Integer; const AMethod: string);
begin
  if AArgs.Length > AMaximum then
    ThrowTypeError(AMethod + ' received unsupported arguments');
end;

procedure ValidateNoOptions(const AValue: TGocciaValue;
  const AMethod: string);
begin
  if not IsMissingValue(AValue) then
    ThrowTypeError(AMethod + ' options are not supported');
end;

function ParseReadOptions(const AValue: TGocciaValue;
  const AMethod: string): Boolean;
var
  EncodingValue: TGocciaValue;
  Encoding: string;
begin
  Result := False;
  if IsMissingValue(AValue) then
    Exit;
  if AValue is TGocciaStringLiteralValue then
  begin
    Encoding := LowerCase(TGocciaStringLiteralValue(AValue).Value);
    if (Encoding <> 'utf8') and (Encoding <> 'utf-8') then
      ThrowTypeError(AMethod + ' only supports utf8 encoding');
    Result := True;
    Exit;
  end;
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be an encoding string or object');

  EncodingValue := TGocciaObjectValue(AValue).GetProperty('encoding');
  if not IsMissingValue(EncodingValue) then
  begin
    if not (EncodingValue is TGocciaStringLiteralValue) then
      ThrowTypeError(AMethod + ' option "encoding" must be a string');
    Encoding := LowerCase(TGocciaStringLiteralValue(EncodingValue).Value);
    if (Encoding <> 'utf8') and (Encoding <> 'utf-8') then
      ThrowTypeError(AMethod + ' only supports utf8 encoding');
    Result := True;
  end;
  if not IsMissingValue(TGocciaObjectValue(AValue).GetProperty('flag')) or
     not IsMissingValue(TGocciaObjectValue(AValue).GetProperty('signal')) then
    ThrowTypeError(AMethod + ' received unsupported options');
end;

function ValidateRecursiveOptions(const AValue: TGocciaValue;
  const AMethod: string): Boolean;
var
  RecursiveValue: TGocciaValue;
  ForceValue: TGocciaValue;
begin
  if IsMissingValue(AValue) then
    Exit(False);
  if AValue is TGocciaBooleanLiteralValue then
    Exit(TGocciaBooleanLiteralValue(AValue).Value);
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be a boolean or object');

  RecursiveValue := TGocciaObjectValue(AValue).GetProperty('recursive');
  if IsMissingValue(RecursiveValue) then
    Result := False
  else
  begin
    if not (RecursiveValue is TGocciaBooleanLiteralValue) then
      ThrowTypeError(AMethod + ' option "recursive" must be a boolean');
    Result := TGocciaBooleanLiteralValue(RecursiveValue).Value;
  end;
  if not IsMissingValue(TGocciaObjectValue(AValue).GetProperty('mode')) then
    ThrowTypeError(AMethod + ' received unsupported options');
  ForceValue := TGocciaObjectValue(AValue).GetProperty('force');
  if not IsMissingValue(ForceValue) then
  begin
    if not (ForceValue is TGocciaBooleanLiteralValue) then
      ThrowTypeError(AMethod + ' option "force" must be a boolean');
    if TGocciaBooleanLiteralValue(ForceValue).Value then
      ThrowTypeError(AMethod + ' received unsupported options');
  end;
end;

procedure ValidateCopyMode(const AValue: TGocciaValue;
  const AMethod: string);
begin
  if IsMissingValue(AValue) then
    Exit;
  if not (AValue is TGocciaNumberLiteralValue) or
     (TGocciaNumberLiteralValue(AValue).Value <> 0) then
    ThrowTypeError(AMethod + ' only supports copy mode 0');
end;

function EnqueueFsCallback(
  const AExtension: TGocciaSandboxRuntimeExtension;
  const AOperation: TGocciaSandboxFsOperation; const APath,
  ADestination: string; const AData: TBytes; const ATextResult,
  ARecursive: Boolean;
  const ACompletion: TGocciaSandboxFsCompletion;
  const ACallback: TGocciaValue): TGocciaValue;
var
  Queue: TGocciaMicrotaskQueue;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  if not Assigned(Queue) then
    raise Exception.Create('Filesystem callbacks require a microtask queue');
  Queue.EnqueueJob(TGocciaSandboxFsJob.Create(AExtension, AOperation,
    APath, ADestination, AData, ATextResult, ARecursive, ACompletion,
    ACallback, nil));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function EnqueueFsPromise(
  const AExtension: TGocciaSandboxRuntimeExtension;
  const AOperation: TGocciaSandboxFsOperation; const APath,
  ADestination: string; const AData: TBytes; const ATextResult,
  ARecursive: Boolean;
  const ACompletion: TGocciaSandboxFsCompletion;
  const AMethod: string): TGocciaValue;
var
  Queue: TGocciaMicrotaskQueue;
  Promise: TGocciaPromiseValue;
  PromiseRoot: TGocciaTempRoot;
begin
  Queue := TGocciaMicrotaskQueue.Instance;
  if not Assigned(Queue) then
    raise Exception.Create(AMethod + ' requires a microtask queue');

  Promise := TGocciaPromiseValue.Create;
  InitializeTempRoot(PromiseRoot);
  try
    AddTempRootIfNeeded(PromiseRoot, Promise);
    Queue.EnqueueJob(TGocciaSandboxFsJob.Create(AExtension, AOperation,
      APath, ADestination, AData, ATextResult, ARecursive, ACompletion,
      nil, Promise));
  finally
    RemoveTempRootIfNeeded(PromiseRoot);
  end;
  Result := Promise;
end;

procedure AppendRunSeed(var AOptions: TGocciaSandboxRunOptions;
  const ASeed: TGocciaSandboxSeedSpec);
var
  Index: Integer;
begin
  Index := Length(AOptions.Seeds);
  SetLength(AOptions.Seeds, Index + 1);
  AOptions.Seeds[Index] := ASeed;
  AOptions.Isolated := True;
end;

function DecodeBase64Bytes(const AText: string): TBytes;
var
  Decoded: string;
begin
  Decoded := DecodeStringBase64(AText);
  SetLength(Result, Length(Decoded));
  if Length(Decoded) > 0 then
    Move(Decoded[1], Result[0], Length(Decoded));
end;

function HasDefinedProperty(const AObject: TGocciaObjectValue;
  const AName: string): Boolean;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(AName);
  Result := Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) and
    not (Value is TGocciaNullLiteralValue);
end;

function IsDirectoryTargetPath(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path = '/') or ((Path <> '') and (Path[Length(Path)] = '/'));
end;

function ObjectStringProperty(const AObject: TGocciaObjectValue;
  const AName, AMethod: string; const ARequired: Boolean): string;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(AName);
  if (not Assigned(Value)) or (Value is TGocciaUndefinedLiteralValue) or
     (Value is TGocciaNullLiteralValue) then
  begin
    if ARequired then
      ThrowTypeError(AMethod + ' requires option "' + AName + '"');
    Exit('');
  end;
  if not (Value is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' option "' + AName + '" must be a string');
  Result := TGocciaStringLiteralValue(Value).Value;
end;

function ObjectBooleanProperty(const AObject: TGocciaObjectValue;
  const AName, AMethod: string): Boolean;
var
  Value: TGocciaValue;
begin
  Value := AObject.GetProperty(AName);
  if (not Assigned(Value)) or (Value is TGocciaUndefinedLiteralValue) or
     (Value is TGocciaNullLiteralValue) then
    Exit(False);
  if not (Value is TGocciaBooleanLiteralValue) then
    ThrowTypeError(AMethod + ' option "' + AName + '" must be a boolean');
  Result := TGocciaBooleanLiteralValue(Value).Value;
end;

procedure AddParentPathSeedFromString(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const ASpec: string);
var
  SeparatorIndex: Integer;
  SourcePath: string;
  TargetPath: string;
  Seed: TGocciaSandboxSeedSpec;
begin
  SeparatorIndex := Pos('=', ASpec);
  if SeparatorIndex > 0 then
  begin
    SourcePath := Copy(ASpec, 1, SeparatorIndex - 1);
    TargetPath := Copy(ASpec, SeparatorIndex + 1, MaxInt);
  end
  else
  begin
    SourcePath := ASpec;
    TargetPath := '';
  end;

  Seed.Kind := sskParentPath;
  Seed.FromPath := AContext.Fs.Normalize(SourcePath,
    AContext.Shell.WorkingDirectory);
  if TargetPath = '' then
    Seed.ToPath := Seed.FromPath
  else
    Seed.ToPath := AContext.Fs.Normalize(TargetPath, '/');
  Seed.ToDirectory := IsDirectoryTargetPath(TargetPath);
  Seed.Path := '';
  Seed.Text := '';
  Seed.Bytes := nil;
  AppendRunSeed(AOptions, Seed);
end;

procedure AddRunSeedFromObject(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const AObject: TGocciaObjectValue;
  const AMethod: string);
var
  Seed: TGocciaSandboxSeedSpec;
  SourceCount: Integer;
begin
  SourceCount := 0;
  if HasDefinedProperty(AObject, 'from') then Inc(SourceCount);
  if HasDefinedProperty(AObject, 'text') then Inc(SourceCount);
  if HasDefinedProperty(AObject, 'base64') then Inc(SourceCount);
  if SourceCount <> 1 then
    ThrowTypeError(AMethod +
      ' seed entries require exactly one of "from", "text", or "base64"');

  Seed.FromPath := '';
  Seed.ToPath := '';
  Seed.ToDirectory := False;
  Seed.Path := '';
  Seed.Text := '';
  Seed.Bytes := nil;

  if HasDefinedProperty(AObject, 'from') then
  begin
    Seed.Kind := sskParentPath;
    Seed.FromPath := AContext.Fs.Normalize(ObjectStringProperty(AObject,
      'from', AMethod, True), AContext.Shell.WorkingDirectory);
    Seed.ToPath := ObjectStringProperty(AObject, 'to', AMethod, False);
    Seed.ToDirectory := IsDirectoryTargetPath(Seed.ToPath);
    if Seed.ToPath = '' then
      Seed.ToPath := Seed.FromPath
    else
      Seed.ToPath := AContext.Fs.Normalize(Seed.ToPath, '/');
    AppendRunSeed(AOptions, Seed);
    Exit;
  end;

  Seed.Path := AContext.Fs.Normalize(ObjectStringProperty(AObject, 'path',
    AMethod, True), '/');
  if HasDefinedProperty(AObject, 'base64') then
  begin
    Seed.Kind := sskBytes;
    Seed.Bytes := DecodeBase64Bytes(ObjectStringProperty(AObject, 'base64',
      AMethod, True));
  end
  else
  begin
    Seed.Kind := sskText;
    Seed.Text := ObjectStringProperty(AObject, 'text', AMethod, True);
  end;
  AppendRunSeed(AOptions, Seed);
end;

procedure AddRunSeedFromValue(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const AValue: TGocciaValue;
  const AMethod: string);
begin
  if AValue is TGocciaStringLiteralValue then
    AddParentPathSeedFromString(AContext, AOptions,
      TGocciaStringLiteralValue(AValue).Value)
  else if AValue is TGocciaObjectValue then
    AddRunSeedFromObject(AContext, AOptions, TGocciaObjectValue(AValue),
      AMethod)
  else
    ThrowTypeError(AMethod + ' seed entries must be strings or objects');
end;

procedure AddRunSeedsFromValue(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const AValue: TGocciaValue;
  const AMethod: string);
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  if (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaNullLiteralValue) then
    Exit;
  if AValue is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(AValue);
    for I := 0 to Arr.GetLength - 1 do
      AddRunSeedFromValue(AContext, AOptions, Arr.GetElement(I), AMethod);
    Exit;
  end;
  AddRunSeedFromValue(AContext, AOptions, AValue, AMethod);
end;

function ParseRunScriptOptions(const AContext: TGocciaSandboxContext;
  const AValue: TGocciaValue; const AMethod: string):
  TGocciaSandboxRunOptions;
var
  OptionsObject: TGocciaObjectValue;
  DiffFormat: string;
begin
  Result := DefaultSandboxRunOptions;
  if (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaNullLiteralValue) then
    Exit;
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be an object');

  OptionsObject := TGocciaObjectValue(AValue);
  if ObjectBooleanProperty(OptionsObject, 'sandbox', AMethod) then
    Result.Isolated := True;
  if ObjectBooleanProperty(OptionsObject, 'diff', AMethod) then
  begin
    Result.IncludeDiff := True;
    Result.Isolated := True;
  end;
  if ObjectBooleanProperty(OptionsObject, 'diffMetadata', AMethod) then
  begin
    Result.DiffMetadata := True;
    Result.IncludeDiff := True;
    Result.Isolated := True;
  end;

  DiffFormat := ObjectStringProperty(OptionsObject, 'diffFormat', AMethod,
    False);
  if DiffFormat <> '' then
    Result.DiffFormat := DiffFormat;
  if (Result.DiffFormat <> 'json') and (Result.DiffFormat <> 'unified') then
    ThrowTypeError(AMethod + ' option "diffFormat" must be "json" or "unified"');

  AddRunSeedsFromValue(AContext, Result, OptionsObject.GetProperty('seed'),
    AMethod);
  AddRunSeedsFromValue(AContext, Result, OptionsObject.GetProperty('seeds'),
    AMethod);
end;

function ShellQuoteValue(const AValue: TGocciaValue): string;
var
  Text: string;
begin
  Text := AValue.ToStringLiteral.Value;
  Text := StringReplace(Text, '''', '''\''''', [rfReplaceAll]);
  Result := '''' + Text + '''';
end;

function BuildCommandLineFromTagArgs(
  const AArgs: TGocciaArgumentsCollection): string;
var
  Template: TGocciaValue;
  TemplateObj: TGocciaObjectValue;
  LengthValue: TGocciaValue;
  TemplateLength: Integer;
  I: Integer;
begin
  if AArgs.Length = 0 then
    Exit('');

  Template := AArgs.GetElement(0);
  if Template is TGocciaStringLiteralValue then
    Exit(TGocciaStringLiteralValue(Template).Value);

  if not (Template is TGocciaObjectValue) then
    ThrowTypeError('$ expects a tagged template or command string');

  TemplateObj := TGocciaObjectValue(Template);
  LengthValue := TemplateObj.GetProperty('length');
  TemplateLength := ToLengthValue(LengthValue);
  Result := '';
  for I := 0 to TemplateLength - 1 do
  begin
    Result := Result + TemplateObj.GetProperty(IntToStr(I)).ToStringLiteral.Value;
    if I + 1 < AArgs.Length then
      Result := Result + ShellQuoteValue(AArgs.GetElement(I + 1));
  end;
end;

{ TGocciaSandboxStatValue }

constructor TGocciaSandboxStatValue.Create(const APrototype: TGocciaObjectValue;
  const AStat: TSandboxFsStat);
begin
  inherited Create(APrototype, 10);
  FKind := AStat.Kind;
  FAtimeMs := SandboxDateTimeToUnixMilliseconds(AStat.AccessedAt);
  FMtimeMs := SandboxDateTimeToUnixMilliseconds(AStat.ModifiedAt);
  FCtimeMs := SandboxDateTimeToUnixMilliseconds(AStat.ChangedAt);
  FBirthtimeMs := SandboxDateTimeToUnixMilliseconds(AStat.BirthTime);
  SetProperty('path', TGocciaStringLiteralValue.Create(AStat.Path));
  SetProperty('name', TGocciaStringLiteralValue.Create(AStat.Name));
  SetProperty('type', TGocciaStringLiteralValue.Create(
    SandboxFsKindName(AStat.Kind)));
  SetProperty('size', TGocciaNumberLiteralValue.Create(AStat.Size));
  SetProperty('atimeMs', TGocciaNumberLiteralValue.Create(FAtimeMs));
  SetProperty('mtimeMs', TGocciaNumberLiteralValue.Create(FMtimeMs));
  SetProperty('ctimeMs', TGocciaNumberLiteralValue.Create(FCtimeMs));
  SetProperty('birthtimeMs', TGocciaNumberLiteralValue.Create(FBirthtimeMs));
end;

{ TGocciaSandboxShellCommandValue }

constructor TGocciaSandboxShellCommandValue.Create(
  const AContext: TGocciaSandboxContext; const ACommandLine: string);
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype, 8);
  FContext := AContext;
  FCommandLine := ACommandLine;
  AddNativeFunction(Self, 'run', Run, 0);
  AddNativeFunction(Self, 'text', Text, 0);
  AddNativeFunction(Self, 'json', Json, 0);
  AddNativeFunction(Self, 'quiet', Quiet, 0);
  AddNativeFunction(Self, 'nothrow', NoThrow, 0);
end;

procedure TGocciaSandboxShellCommandValue.EnsureExecuted;
begin
  if FExecuted then
    Exit;
  FResult := FContext.Shell.Exec(FCommandLine);
  FExecuted := True;
  if (FResult.ExitCode <> 0) and not FNoThrow then
    ThrowTypeError('Shell command failed with exit code ' +
      IntToStr(FResult.ExitCode) + ': ' + Trim(FResult.ErrorOutput));
end;

function TGocciaSandboxShellCommandValue.ResultObject: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    4);
  Result.SetProperty('exitCode',
    TGocciaNumberLiteralValue.Create(FResult.ExitCode));
  if FQuiet then
  begin
    Result.SetProperty('stdout', TGocciaStringLiteralValue.Create(''));
    Result.SetProperty('stderr', TGocciaStringLiteralValue.Create(''));
  end
  else
  begin
    Result.SetProperty('stdout',
      TGocciaStringLiteralValue.Create(FResult.Output));
    Result.SetProperty('stderr',
      TGocciaStringLiteralValue.Create(FResult.ErrorOutput));
  end;
  Result.SetProperty('ok',
    TGocciaBooleanLiteralValue.Create(FResult.ExitCode = 0));
end;

function TGocciaSandboxShellCommandValue.Fulfilled(
  const AValue: TGocciaValue): TGocciaPromiseValue;
begin
  Result := FulfilledPromise(AValue);
end;

function TGocciaSandboxShellCommandValue.Run(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  try
    EnsureExecuted;
    Result := Fulfilled(ResultObject);
  except
    on E: TGocciaThrowValue do
      Result := RejectedPromise(E.Value);
    on E: Exception do
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
  end;
end;

function TGocciaSandboxShellCommandValue.Text(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  try
    EnsureExecuted;
    if FQuiet then
      Result := Fulfilled(TGocciaStringLiteralValue.Create(''))
    else
      Result := Fulfilled(TGocciaStringLiteralValue.Create(FResult.Output));
  except
    on E: TGocciaThrowValue do
      Result := RejectedPromise(E.Value);
    on E: Exception do
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
  end;
end;

function TGocciaSandboxShellCommandValue.Json(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Parser: TGocciaJSONParser;
  Parsed: TGocciaValue;
begin
  try
    EnsureExecuted;
    Parser := TGocciaJSONParser.Create;
    try
      Parsed := Parser.Parse(UTF8String(FResult.Output));
    finally
      Parser.Free;
    end;
    Result := Fulfilled(Parsed);
  except
    on E: TGocciaThrowValue do
      Result := RejectedPromise(E.Value);
    on E: Exception do
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
  end;
end;

function TGocciaSandboxShellCommandValue.Quiet(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FQuiet := True;
  Result := Self;
end;

function TGocciaSandboxShellCommandValue.NoThrow(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FNoThrow := True;
  Result := Self;
end;

{ TGocciaSandboxRuntimeExtension }

function RequireSandboxStatsReceiver(
  const AThisValue: TGocciaValue): TGocciaSandboxStatValue;
begin
  if not (AThisValue is TGocciaSandboxStatValue) then
  begin
    ThrowTypeError('Stats method called on incompatible receiver');
    Exit(nil);
  end;
  Result := TGocciaSandboxStatValue(AThisValue);
end;

function TGocciaSandboxRuntimeExtension.EnsureStatsPrototype:
  TGocciaObjectValue;
var
  Realm: TGocciaRealm;

  procedure DefineMethod(const AName: string;
    const ACallback: TGocciaNativeFunctionCallback);
  var
    MethodValue: TGocciaNativeFunctionValue;
  begin
    MethodValue := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ACallback, AName, 0);
    Result.DefineProperty(AName, TGocciaPropertyDescriptorData.Create(
      MethodValue, [pfWritable, pfConfigurable]));
  end;

  procedure DefineDateAccessor(const AName: string;
    const ACallback: TGocciaNativeFunctionCallback);
  var
    Getter: TGocciaNativeFunctionValue;
  begin
    Getter := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      ACallback, 'get ' + AName, 0);
    Result.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(
      Getter, nil, [pfEnumerable, pfConfigurable]));
  end;

begin
  Realm := CurrentRealm;
  if not Assigned(Realm) then
    raise Exception.Create('Sandbox Stats prototype requires an active realm');

  Result := TGocciaObjectValue(
    Realm.GetSlot(GSandboxStatsPrototypeSlot));
  if Assigned(Result) then
    Exit;

  Result := TGocciaObjectValue.Create(
    TGocciaObjectValue.GetSharedObjectPrototypeForRealm(Realm), 7);
  DefineDateAccessor('atime', StatsAtime);
  DefineDateAccessor('mtime', StatsMtime);
  DefineDateAccessor('ctime', StatsCtime);
  DefineDateAccessor('birthtime', StatsBirthtime);
  DefineMethod('isFile', StatsIsFile);
  DefineMethod('isDirectory', StatsIsDirectory);
  DefineMethod('isSymbolicLink', StatsIsSymbolicLink);
  Realm.SetSlot(GSandboxStatsPrototypeSlot, Result);
end;

function TGocciaSandboxRuntimeExtension.CreateStatsValue(
  const AStat: TSandboxFsStat): TGocciaValue;
begin
  Result := TGocciaSandboxStatValue.Create(EnsureStatsPrototype, AStat);
end;

function TGocciaSandboxRuntimeExtension.CreateStatsDate(
  const AMilliseconds: Double): TGocciaValue;
var
  DateConstructor: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  Fraction: Double;
  RoundedMilliseconds: Double;
begin
  DateConstructor := GetDateIntrinsic(Runtime.Engine.Interpreter);
  Fraction := Frac(AMilliseconds);
  RoundedMilliseconds := AMilliseconds - Fraction;
  if Fraction >= 0.5 then
    RoundedMilliseconds := RoundedMilliseconds + 1
  else if Fraction < -0.5 then
    RoundedMilliseconds := RoundedMilliseconds - 1;
  Args := TGocciaArgumentsCollection.Create([
    TGocciaNumberLiteralValue.Create(RoundedMilliseconds)]);
  try
    Result := ConstructValue(DateConstructor, Args, DateConstructor);
  finally
    Args.Free;
  end;
end;

function TGocciaSandboxRuntimeExtension.StatsAtime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateStatsDate(
    RequireSandboxStatsReceiver(AThisValue).FAtimeMs);
end;

function TGocciaSandboxRuntimeExtension.StatsMtime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateStatsDate(
    RequireSandboxStatsReceiver(AThisValue).FMtimeMs);
end;

function TGocciaSandboxRuntimeExtension.StatsCtime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateStatsDate(
    RequireSandboxStatsReceiver(AThisValue).FCtimeMs);
end;

function TGocciaSandboxRuntimeExtension.StatsBirthtime(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := CreateStatsDate(
    RequireSandboxStatsReceiver(AThisValue).FBirthtimeMs);
end;

function TGocciaSandboxRuntimeExtension.StatsIsFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    RequireSandboxStatsReceiver(AThisValue).FKind = nkFile);
end;

function TGocciaSandboxRuntimeExtension.StatsIsDirectory(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    RequireSandboxStatsReceiver(AThisValue).FKind = nkDirectory);
end;

function TGocciaSandboxRuntimeExtension.StatsIsSymbolicLink(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  RequireSandboxStatsReceiver(AThisValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

constructor TGocciaSandboxRuntimeExtension.Create(
  const AContext: TGocciaSandboxContext);
begin
  inherited Create;
  FContext := AContext;
end;

procedure TGocciaSandboxRuntimeExtension.Attach(
  const ARuntime: TGocciaRuntimeCore);
var
  FsNamespace: TGocciaObjectValue;
  GocciaNamespace: TGocciaObjectValue;
begin
  inherited Attach(ARuntime);
  FsNamespace := CreateFsNamespace;
  GocciaNamespace := CreateGocciaNamespace;

  FFsModule := TGocciaModule.Create('fs');
  FFsModule.AddExportValue(KEYWORD_DEFAULT, FsNamespace);
  FFsModule.AddExportValue('readFile', FsNamespace.GetProperty('readFile'));
  FFsModule.AddExportValue('writeFile', FsNamespace.GetProperty('writeFile'));
  FFsModule.AddExportValue('appendFile',
    FsNamespace.GetProperty('appendFile'));
  FFsModule.AddExportValue('mkdir', FsNamespace.GetProperty('mkdir'));
  FFsModule.AddExportValue('readdir', FsNamespace.GetProperty('readdir'));
  FFsModule.AddExportValue('stat', FsNamespace.GetProperty('stat'));
  FFsModule.AddExportValue('exists', FsNamespace.GetProperty('exists'));
  FFsModule.AddExportValue('rm', FsNamespace.GetProperty('rm'));
  FFsModule.AddExportValue('rename', FsNamespace.GetProperty('rename'));
  FFsModule.AddExportValue('copyFile', FsNamespace.GetProperty('copyFile'));
  FFsModule.AddExportValue('readFileSync',
    FsNamespace.GetProperty('readFileSync'));
  FFsModule.AddExportValue('writeFileSync',
    FsNamespace.GetProperty('writeFileSync'));
  FFsModule.AddExportValue('appendFileSync',
    FsNamespace.GetProperty('appendFileSync'));
  FFsModule.AddExportValue('mkdirSync', FsNamespace.GetProperty('mkdirSync'));
  FFsModule.AddExportValue('readdirSync',
    FsNamespace.GetProperty('readdirSync'));
  FFsModule.AddExportValue('statSync', FsNamespace.GetProperty('statSync'));
  FFsModule.AddExportValue('existsSync',
    FsNamespace.GetProperty('existsSync'));
  FFsModule.AddExportValue('rmSync', FsNamespace.GetProperty('rmSync'));
  FFsModule.AddExportValue('renameSync',
    FsNamespace.GetProperty('renameSync'));
  FFsModule.AddExportValue('copyFileSync',
    FsNamespace.GetProperty('copyFileSync'));
  FFsModule.AddExportValue('promises', FsNamespace.GetProperty('promises'));
  Runtime.Engine.ModuleLoader.GlobalModules.Add('fs', FFsModule);

  FGocciaModule := TGocciaModule.Create('goccia');
  FGocciaModule.AddExportValue(KEYWORD_DEFAULT, GocciaNamespace);
  FGocciaModule.AddExportValue('$', GocciaNamespace.GetProperty('$'));
  FGocciaModule.AddExportValue('runScript',
    GocciaNamespace.GetProperty('runScript'));
  Runtime.Engine.ModuleLoader.GlobalModules.Add('goccia', FGocciaModule);
end;

procedure TGocciaSandboxRuntimeExtension.Detach;
begin
  if Assigned(Runtime) and Assigned(Runtime.Engine) then
  begin
    Runtime.Engine.ModuleLoader.GlobalModules.Remove('goccia');
    Runtime.Engine.ModuleLoader.GlobalModules.Remove('fs');
  end;
  FGocciaModule.Free;
  FGocciaModule := nil;
  FFsModule.Free;
  FFsModule := nil;
  inherited;
end;

function TGocciaSandboxRuntimeExtension.CreateFsNamespace: TGocciaObjectValue;
var
  Promises: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    28);
  AddNativeFunction(Result, 'readFileSync', FsReadFileSync, 2);
  AddNativeFunction(Result, 'writeFileSync', FsWriteFileSync, 2);
  AddNativeFunction(Result, 'appendFileSync', FsAppendFileSync, 2);
  AddNativeFunction(Result, 'mkdirSync', FsMkdirSync, 2);
  AddNativeFunction(Result, 'readdirSync', FsReaddirSync, 1);
  AddNativeFunction(Result, 'statSync', FsStatSync, 1);
  AddNativeFunction(Result, 'existsSync', FsExistsSync, 1);
  AddNativeFunction(Result, 'rmSync', FsRmSync, 2);
  AddNativeFunction(Result, 'renameSync', FsRenameSync, 2);
  AddNativeFunction(Result, 'copyFileSync', FsCopyFileSync, 2);

  AddNativeFunction(Result, 'readFile', FsReadFile, 3);
  AddNativeFunction(Result, 'writeFile', FsWriteFile, 4);
  AddNativeFunction(Result, 'appendFile', FsAppendFile, 4);
  AddNativeFunction(Result, 'mkdir', FsMkdir, 3);
  AddNativeFunction(Result, 'readdir', FsReaddir, 3);
  AddNativeFunction(Result, 'stat', FsStat, 1);
  AddNativeFunction(Result, 'exists', FsExists, 2);
  AddNativeFunction(Result, 'rm', FsRm, 3);
  AddNativeFunction(Result, 'rename', FsRename, 3);
  AddNativeFunction(Result, 'copyFile', FsCopyFile, 4);

  Promises := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    12);
  AddNativeFunction(Promises, 'readFile', FsPromisesReadFile, 2);
  AddNativeFunction(Promises, 'writeFile', FsPromisesWriteFile, 2);
  AddNativeFunction(Promises, 'appendFile', FsPromisesAppendFile, 2);
  AddNativeFunction(Promises, 'mkdir', FsPromisesMkdir, 2);
  AddNativeFunction(Promises, 'readdir', FsPromisesReaddir, 1);
  AddNativeFunction(Promises, 'stat', FsPromisesStat, 1);
  AddNativeFunction(Promises, 'rm', FsPromisesRm, 2);
  AddNativeFunction(Promises, 'rename', FsPromisesRename, 2);
  AddNativeFunction(Promises, 'copyFile', FsPromisesCopyFile, 2);
  Result.SetProperty('promises', Promises);
end;

function TGocciaSandboxRuntimeExtension.CreateGocciaNamespace:
  TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    4);
  AddNativeFunction(Result, '$', SandboxDollar, -1);
  AddNativeFunction(Result, 'runScript', RunScript, 2);
end;

function TGocciaSandboxRuntimeExtension.SandboxDollar(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSandboxShellCommandValue.Create(FContext,
    BuildCommandLineFromTagArgs(AArgs));
end;

function TGocciaSandboxRuntimeExtension.RunScript(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  EntryPath: string;
  Options: TGocciaSandboxRunOptions;
  RunResult: TGocciaSandboxRunResult;
  Obj: TGocciaObjectValue;
begin
  EntryPath := RequireStringArg(AArgs, 0, 'runScript');
  if not Assigned(FContext.RunScriptCallback) then
    ThrowTypeError('runScript is not configured');

  Options := ParseRunScriptOptions(FContext, AArgs.GetElement(1), 'runScript');
  RunResult := FContext.RunScriptCallback(FContext, FContext.Fs.Normalize(
    EntryPath, FContext.Shell.WorkingDirectory), Options);
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype, 8);
  Obj.SetProperty('ok', TGocciaBooleanLiteralValue.Create(RunResult.Ok));
  Obj.SetProperty('exitCode',
    TGocciaNumberLiteralValue.Create(RunResult.ExitCode));
  Obj.SetProperty('stdout', TGocciaStringLiteralValue.Create(RunResult.Output));
  Obj.SetProperty('stderr',
    TGocciaStringLiteralValue.Create(RunResult.ErrorOutput));
  if Assigned(RunResult.ResultValue) then
    Obj.SetProperty('result', RunResult.ResultValue)
  else
    Obj.SetProperty('result', TGocciaNullLiteralValue.NullValue);
  if RunResult.ErrorMessage <> '' then
    Obj.SetProperty('error',
      TGocciaStringLiteralValue.Create(RunResult.ErrorMessage))
  else
    Obj.SetProperty('error', TGocciaNullLiteralValue.NullValue);
  if RunResult.DiffRequested then
    Obj.SetProperty('diff', TGocciaStringLiteralValue.Create(RunResult.Diff))
  else
    Obj.SetProperty('diff', TGocciaNullLiteralValue.NullValue);
  Result := Obj;
end;

function TGocciaSandboxRuntimeExtension.FsReadFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.readFileSync');
  Result := ReadFileValue(Path, WantsTextResult(AArgs.GetElement(1)));
end;

function TGocciaSandboxRuntimeExtension.FsWriteFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Bytes: TBytes;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.writeFileSync');
  Bytes := ValueToBytes(AArgs.GetElement(1), 'fs.writeFileSync');
  Result := WriteFileValue(Path, Bytes);
end;

function TGocciaSandboxRuntimeExtension.FsAppendFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Bytes: TBytes;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.appendFileSync');
  Bytes := ValueToBytes(AArgs.GetElement(1), 'fs.appendFileSync');
  Result := AppendFileValue(Path, Bytes);
end;

function TGocciaSandboxRuntimeExtension.FsMkdirSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Recursive: Boolean;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.mkdirSync');
  Recursive := OptionRecursive(AArgs.GetElement(1));
  Result := MkdirValue(Path, Recursive, False);
end;

function TGocciaSandboxRuntimeExtension.FsReaddirSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.readdirSync');
  Result := ReaddirValue(Path);
end;

function TGocciaSandboxRuntimeExtension.FsStatSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.statSync');
  Result := StatValue(Path);
end;

function TGocciaSandboxRuntimeExtension.FsExistsSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.existsSync');
  Result := ExistsValue(Path);
end;

function TGocciaSandboxRuntimeExtension.FsRmSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Recursive: Boolean;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.rmSync');
  Recursive := OptionRecursive(AArgs.GetElement(1));
  Result := RmValue(Path, Recursive);
end;

function TGocciaSandboxRuntimeExtension.FsRenameSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.renameSync');
  Destination := RequireStringArg(AArgs, 1, 'fs.renameSync');
  Result := RenameValue(Path, Destination);
end;

function TGocciaSandboxRuntimeExtension.FsCopyFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.copyFileSync');
  Destination := RequireStringArg(AArgs, 1, 'fs.copyFileSync');
  Result := CopyFileValue(Path, Destination);
end;

function TGocciaSandboxRuntimeExtension.ReadFileValue(const APath: string;
  const ATextResult: Boolean): TGocciaValue;
begin
  try
    if ATextResult then
      Exit(TGocciaStringLiteralValue.Create(FContext.Fs.ReadAllText(APath)));
    Result := BytesToUint8Array(FContext.Fs.ReadAllBytes(APath));
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'readFile', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.WriteFileValue(const APath: string;
  const AData: TBytes): TGocciaValue;
begin
  try
    FContext.Fs.WriteAllBytes(APath, AData);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'writeFile', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.AppendFileValue(const APath: string;
  const AData: TBytes): TGocciaValue;
var
  Existing: TBytes;
  Combined: TBytes;
begin
  Existing := nil;
  try
    if FContext.Fs.Exists(APath) then
      Existing := FContext.Fs.ReadAllBytes(APath);
    SetLength(Combined, Length(Existing) + Length(AData));
    if Length(Existing) > 0 then
      Move(Existing[0], Combined[0], Length(Existing));
    if Length(AData) > 0 then
      Move(AData[0], Combined[Length(Existing)], Length(AData));
    FContext.Fs.WriteAllBytes(APath, Combined);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'appendFile', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.MkdirValue(const APath: string;
  const ARecursive, AReturnFirstCreatedPath: Boolean): TGocciaValue;
var
  FirstCreatedPath: string;
begin
  try
    FirstCreatedPath := FContext.Fs.MakeDirectoryWithResult(APath, ARecursive);
    if AReturnFirstCreatedPath and ARecursive and
       (FirstCreatedPath <> '') then
      Result := TGocciaStringLiteralValue.Create(FirstCreatedPath)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'mkdir', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.ReaddirValue(
  const APath: string): TGocciaValue;
var
  Entries: TSandboxFsStatArray;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  try
    Entries := FContext.Fs.List(APath);
    Arr := TGocciaArrayValue.Create(nil, Length(Entries));
    for I := 0 to High(Entries) do
      Arr.SetElement(I, TGocciaStringLiteralValue.Create(Entries[I].Name));
    Result := Arr;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'readdir', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.StatValue(
  const APath: string): TGocciaValue;
begin
  try
    Result := CreateStatsValue(FContext.Fs.Stat(APath));
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'stat', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.ExistsValue(
  const APath: string): TGocciaValue;
begin
  try
    Result := TGocciaBooleanLiteralValue.Create(FContext.Fs.Exists(APath));
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'exists', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.RmValue(const APath: string;
  const ARecursive: Boolean): TGocciaValue;
begin
  try
    FContext.Fs.DeletePath(APath, ARecursive);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'rm', APath));
  end;
end;

function TGocciaSandboxRuntimeExtension.RenameValue(const APath,
  ADestination: string): TGocciaValue;
begin
  try
    FContext.Fs.MovePath(APath, ADestination);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(CreateSandboxFileSystemError(E,
        'rename', APath, ADestination));
  end;
end;

function TGocciaSandboxRuntimeExtension.CopyFileValue(const APath,
  ADestination: string): TGocciaValue;
begin
  try
    FContext.Fs.CopyPath(APath, ADestination, False);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(CreateSandboxFileSystemError(E,
        'copyFile', APath, ADestination));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsReadFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Callback: TGocciaValue;
  TextResult: Boolean;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.readFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.readFile');
  if AArgs.Length >= 3 then
  begin
    OptionsValue := AArgs.GetElement(1);
    Callback := RequireFsCallback(AArgs, 2, 'fs.readFile');
  end
  else
  begin
    OptionsValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Callback := RequireFsCallback(AArgs, 1, 'fs.readFile');
  end;
  TextResult := ParseReadOptions(OptionsValue, 'fs.readFile');
  Result := EnqueueFsCallback(Self, sfoReadFile, Path, '', nil,
    TextResult, False, sfcValue, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsWriteFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Data: TBytes;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 4, 'fs.writeFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.writeFile');
  Data := RequireAsyncData(AArgs, 1, 'fs.writeFile');
  if AArgs.Length >= 4 then
  begin
    ValidateNoOptions(AArgs.GetElement(2), 'fs.writeFile');
    Callback := RequireFsCallback(AArgs, 3, 'fs.writeFile');
  end
  else
    Callback := RequireFsCallback(AArgs, 2, 'fs.writeFile');
  Result := EnqueueFsCallback(Self, sfoWriteFile, Path, '', Data,
    False, False, sfcErrorOnly, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsAppendFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Data: TBytes;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 4, 'fs.appendFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.appendFile');
  Data := RequireAsyncData(AArgs, 1, 'fs.appendFile');
  if AArgs.Length >= 4 then
  begin
    ValidateNoOptions(AArgs.GetElement(2), 'fs.appendFile');
    Callback := RequireFsCallback(AArgs, 3, 'fs.appendFile');
  end
  else
    Callback := RequireFsCallback(AArgs, 2, 'fs.appendFile');
  Result := EnqueueFsCallback(Self, sfoAppendFile, Path, '', Data,
    False, False, sfcErrorOnly, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsMkdir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Callback: TGocciaValue;
  Recursive: Boolean;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.mkdir');
  Path := RequireAsyncPath(AArgs, 0, 'fs.mkdir');
  if AArgs.Length >= 3 then
  begin
    OptionsValue := AArgs.GetElement(1);
    Callback := RequireFsCallback(AArgs, 2, 'fs.mkdir');
  end
  else
  begin
    OptionsValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Callback := RequireFsCallback(AArgs, 1, 'fs.mkdir');
  end;
  Recursive := ValidateRecursiveOptions(OptionsValue, 'fs.mkdir');
  Result := EnqueueFsCallback(Self, sfoMkdir, Path, '', nil, False,
    Recursive, sfcMkdir, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsReaddir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.readdir');
  Path := RequireAsyncPath(AArgs, 0, 'fs.readdir');
  if AArgs.Length >= 3 then
  begin
    ValidateNoOptions(AArgs.GetElement(1), 'fs.readdir');
    Callback := RequireFsCallback(AArgs, 2, 'fs.readdir');
  end
  else
    Callback := RequireFsCallback(AArgs, 1, 'fs.readdir');
  Result := EnqueueFsCallback(Self, sfoReaddir, Path, '', nil, False,
    False, sfcValue, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsStat(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.stat');
  Path := RequireAsyncPath(AArgs, 0, 'fs.stat');
  if AArgs.Length >= 3 then
  begin
    ValidateNoOptions(AArgs.GetElement(1), 'fs.stat');
    Callback := RequireFsCallback(AArgs, 2, 'fs.stat');
  end
  else
    Callback := RequireFsCallback(AArgs, 1, 'fs.stat');
  Result := EnqueueFsCallback(Self, sfoStat, Path, '', nil, False,
    False, sfcValue, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsExists(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.exists');
  Path := RequireAsyncPath(AArgs, 0, 'fs.exists');
  Callback := RequireFsCallback(AArgs, 1, 'fs.exists');
  Result := EnqueueFsCallback(Self, sfoExists, Path, '', nil, False,
    False, sfcExists, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsRm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Callback: TGocciaValue;
  Recursive: Boolean;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.rm');
  Path := RequireAsyncPath(AArgs, 0, 'fs.rm');
  if AArgs.Length >= 3 then
  begin
    OptionsValue := AArgs.GetElement(1);
    Callback := RequireFsCallback(AArgs, 2, 'fs.rm');
  end
  else
  begin
    OptionsValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Callback := RequireFsCallback(AArgs, 1, 'fs.rm');
  end;
  Recursive := ValidateRecursiveOptions(OptionsValue, 'fs.rm');
  Result := EnqueueFsCallback(Self, sfoRm, Path, '', nil, False,
    Recursive, sfcErrorOnly, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsRename(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.rename');
  Path := RequireAsyncPath(AArgs, 0, 'fs.rename');
  Destination := RequireAsyncPath(AArgs, 1, 'fs.rename');
  Callback := RequireFsCallback(AArgs, 2, 'fs.rename');
  Result := EnqueueFsCallback(Self, sfoRename, Path, Destination, nil,
    False, False, sfcErrorOnly, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsCopyFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
  Callback: TGocciaValue;
begin
  ValidateArgumentCount(AArgs, 4, 'fs.copyFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.copyFile');
  Destination := RequireAsyncPath(AArgs, 1, 'fs.copyFile');
  if AArgs.Length >= 4 then
  begin
    ValidateCopyMode(AArgs.GetElement(2), 'fs.copyFile');
    Callback := RequireFsCallback(AArgs, 3, 'fs.copyFile');
  end
  else
    Callback := RequireFsCallback(AArgs, 2, 'fs.copyFile');
  Result := EnqueueFsCallback(Self, sfoCopyFile, Path, Destination, nil,
    False, False, sfcErrorOnly, Callback);
end;

function TGocciaSandboxRuntimeExtension.FsPromisesReadFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  TextResult: Boolean;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.readFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.readFile');
  OptionsValue := AArgs.GetElement(1);
  TextResult := ParseReadOptions(OptionsValue, 'fs.promises.readFile');
  Result := EnqueueFsPromise(Self, sfoReadFile, Path, '', nil,
    TextResult, False, sfcValue, 'fs.promises.readFile');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesWriteFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Data: TBytes;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.promises.writeFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.writeFile');
  Data := RequireAsyncData(AArgs, 1, 'fs.promises.writeFile');
  ValidateNoOptions(AArgs.GetElement(2), 'fs.promises.writeFile');
  Result := EnqueueFsPromise(Self, sfoWriteFile, Path, '', Data,
    False, False, sfcErrorOnly, 'fs.promises.writeFile');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesAppendFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Data: TBytes;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.promises.appendFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.appendFile');
  Data := RequireAsyncData(AArgs, 1, 'fs.promises.appendFile');
  ValidateNoOptions(AArgs.GetElement(2), 'fs.promises.appendFile');
  Result := EnqueueFsPromise(Self, sfoAppendFile, Path, '', Data,
    False, False, sfcErrorOnly, 'fs.promises.appendFile');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesMkdir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Recursive: Boolean;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.mkdir');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.mkdir');
  OptionsValue := AArgs.GetElement(1);
  Recursive := ValidateRecursiveOptions(OptionsValue, 'fs.promises.mkdir');
  Result := EnqueueFsPromise(Self, sfoMkdir, Path, '', nil, False,
    Recursive, sfcMkdir, 'fs.promises.mkdir');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesReaddir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.readdir');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.readdir');
  ValidateNoOptions(AArgs.GetElement(1), 'fs.promises.readdir');
  Result := EnqueueFsPromise(Self, sfoReaddir, Path, '', nil, False,
    False, sfcValue, 'fs.promises.readdir');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesStat(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.stat');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.stat');
  ValidateNoOptions(AArgs.GetElement(1), 'fs.promises.stat');
  Result := EnqueueFsPromise(Self, sfoStat, Path, '', nil, False,
    False, sfcValue, 'fs.promises.stat');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesRm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Recursive: Boolean;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.rm');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.rm');
  OptionsValue := AArgs.GetElement(1);
  Recursive := ValidateRecursiveOptions(OptionsValue, 'fs.promises.rm');
  Result := EnqueueFsPromise(Self, sfoRm, Path, '', nil, False,
    Recursive, sfcErrorOnly, 'fs.promises.rm');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesRename(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.rename');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.rename');
  Destination := RequireAsyncPath(AArgs, 1, 'fs.promises.rename');
  Result := EnqueueFsPromise(Self, sfoRename, Path, Destination, nil,
    False, False, sfcErrorOnly, 'fs.promises.rename');
end;

function TGocciaSandboxRuntimeExtension.FsPromisesCopyFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.promises.copyFile');
  Path := RequireAsyncPath(AArgs, 0, 'fs.promises.copyFile');
  Destination := RequireAsyncPath(AArgs, 1, 'fs.promises.copyFile');
  ValidateCopyMode(AArgs.GetElement(2), 'fs.promises.copyFile');
  Result := EnqueueFsPromise(Self, sfoCopyFile, Path, Destination, nil,
    False, False, sfcErrorOnly, 'fs.promises.copyFile');
end;

initialization
  GSandboxStatsPrototypeSlot :=
    RegisterRealmSlot('Sandbox.fs.Stats.prototype');

end.
