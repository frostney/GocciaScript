unit Goccia.RuntimeExtensions.Sandbox;

{$I Goccia.inc}

interface

uses
  SysUtils,

  SandboxGlob,
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
    FFsModuleRegistered: Boolean;
    FGocciaModule: TGocciaModule;
    FGocciaModuleRegistered: Boolean;
    FPreviousFsModule: TGocciaModule;
    FPreviousGocciaModule: TGocciaModule;
    FPreviousRootClampCallback: TSandboxRootClampCallback;
    FRootClampInstalled: Boolean;
    FSearchModule: TGocciaModule;
    FSearchModuleRegistered: Boolean;
    FPreviousSearchModule: TGocciaModule;

    procedure HandleRootClamp(const APath, ABase,
      ACanonicalPath: string);
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

    procedure SearchProgress;
    function GlobPaths(const APatterns, AExcludes: TSandboxGlobPatternArray;
      const ACwd: string): TStringArray;
    function FsGlobSync(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsGlob(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsPromisesGlob(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SearchGrep(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function SearchFuzzyFind(const AArgs: TGocciaArgumentsCollection;
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
  Math,

  SandboxFuzzy,
  SandboxShell,
  TextEncoding,

  Goccia.Base64,
  Goccia.CapabilityAudit,
  Goccia.Constants.ErrorNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.MicrotaskQueue,
  Goccia.NativeLimits,
  Goccia.Realm,
  Goccia.Sandbox.FileSystemErrors,
  Goccia.Sandbox.Search,
  Goccia.Shims,
  Goccia.UncatchableFault,
  Goccia.Utils,
  Goccia.Utils.Arrays,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.DataViewValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue,
  Goccia.VM.Exception;

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
    sfoCopyFile,
    sfoGlob
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
    FGlobPatterns: TSandboxGlobPatternArray;
    FGlobExcludes: TSandboxGlobPatternArray;
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

  { What fs.promises.glob returns: an async iterator over the paths, which
    are found when the first next() runs. }
  TGocciaSandboxGlobIteratorValue = class(TGocciaObjectValue)
  private
    FExtension: TGocciaSandboxRuntimeExtension;
    FPatterns: TSandboxGlobPatternArray;
    FExcludes: TSandboxGlobPatternArray;
    FCwd: string;
    FPaths: TStringArray;
    FStarted: Boolean;
    FIndex: Integer;
    function IteratorResult(const AValue: TGocciaValue;
      const ADone: Boolean): TGocciaValue;
  public
    constructor Create(const AExtension: TGocciaSandboxRuntimeExtension;
      const APatterns, AExcludes: TSandboxGlobPatternArray;
      const ACwd: string);
    function Next(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ReturnMethod(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function AsyncIterator(const AArgs: TGocciaArgumentsCollection;
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

function RejectedPromiseFromException(
  const AException: Exception): TGocciaPromiseValue;
begin
  { Opacity is decided before conversion, because everything below this test
    turns a Pascal exception into something `.catch` can absorb.

    This runs inside the `on E: Exception` handler that caught AException, but
    not lexically, so a bare `raise` will not compile. `raise AException`
    re-raised the object by name, which starts a second propagation of an
    exception the enclosing handler still owns and frees on exit — the dangling
    re-raise behind the spurious access violations on the async paths (see the
    same note at PromiseRejectionReasonFromException in
    Goccia.Builtins.GlobalPromise.pas). AcquireExceptionObject takes a
    reference to the in-flight exception so the enclosing handler no longer
    frees it out from under this re-raise. }
  if IsUncatchableFault(AException) then
    raise Exception(AcquireExceptionObject)
  else if AException is EGocciaBytecodeThrow then
    Result := RejectedPromise(EGocciaBytecodeThrow(AException).ThrownValue)
  else if AException is TGocciaThrowValue then
    Result := RejectedPromise(TGocciaThrowValue(AException).Value)
  else
    Result := RejectedPromise(
      CreateErrorObject(ERROR_NAME, AException.Message));
end;

{ The paths as a script array, rooted while the strings are allocated. }
function PathsToArray(const APaths: TStringArray): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  ArrRoot: TGocciaTempRoot;
  Index: Integer;
begin
  Arr := TGocciaArrayValue.Create(nil, Length(APaths));
  InitializeTempRoot(ArrRoot);
  try
    AddTempRootIfNeeded(ArrRoot, Arr);
    for Index := 0 to High(APaths) do
      ArrayCreateDataProperty(Arr, Index,
        TGocciaStringLiteralValue.Create(APaths[Index]));
  finally
    RemoveTempRootIfNeeded(ArrRoot);
  end;
  Result := Arr;
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
      sfoGlob:
        Value := PathsToArray(FExtension.GlobPaths(FGlobPatterns,
          FGlobExcludes, FPath));
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
      { CompleteFailure settles the promise the guest is awaiting (or calls its
        callback with an Error), so absorbing a ceiling or an integrity fault
        here would hand it straight to `catch`. The job runs from
        TGocciaMicrotaskQueue.ExecuteTask on the engine's own thread, and the
        task carries no result promise, so every arm there re-raises too and
        the fault keeps unwinding out of DrainQueue to the host. }
      if IsUncatchableFault(E) then
        raise;
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

// A Uint8Array over ABytes itself: the new ArrayBuffer adopts the array and
// charges the memory limit as an allocation of the same length would, so the
// bytes are neither zero-filled nor copied. The caller must not use ABytes
// afterwards.
function BytesToUint8Array(const ABytes: TBytes): TGocciaTypedArrayValue;
var
  Buffer: TGocciaArrayBufferValue;
begin
  if Length(ABytes) > High(Integer) then
    ThrowRangeError(SErrorInvalidTypedArrayLength, SSuggestTypedArrayLength);
  Buffer := TGocciaArrayBufferValue.CreateFromBytes(ABytes);
  Result := TGocciaTypedArrayValue.Create(takUint8, Buffer, 0,
    Length(ABytes));
end;

// A growable SharedArrayBuffer's block is longer than its byte length.
function ArrayBufferViewBufferByteLength(const ABuffer: TGocciaValue): Integer;
begin
  if ABuffer is TGocciaArrayBufferValue then
    Result := System.Length(TGocciaArrayBufferValue(ABuffer).Data)
  else if ABuffer is TGocciaSharedArrayBufferValue then
    Result := TGocciaSharedArrayBufferValue(ABuffer).ByteLength
  else
    Result := 0;
end;

function ArrayBufferViewBufferData(const ABuffer: TGocciaValue): TBytes;
begin
  if ABuffer is TGocciaArrayBufferValue then
    Result := TGocciaArrayBufferValue(ABuffer).Data
  else if ABuffer is TGocciaSharedArrayBufferValue then
    Result := TGocciaSharedArrayBufferValue(ABuffer).Data
  else
    Result := nil;
end;

// The bytes a TypedArray or DataView views, as Node writes them: the view's
// byte length from its byte offset, not one byte per element. An
// out-of-bounds or detached TypedArray has no bytes; such a DataView throws,
// as its byteLength getter does. The length is read before the buffer data,
// so a view whose resizable buffer grew copies from the current data rather
// than the smaller one it last saw.
function TryGetArrayBufferViewBytes(const AValue: TGocciaValue;
  out ABytes: TBytes): Boolean;
var
  TA: TGocciaTypedArrayValue;
  View: TGocciaDataViewValue;
  Data: TBytes;
  Offset, ByteLength: Integer;
begin
  ABytes := nil;
  if AValue is TGocciaTypedArrayValue then
  begin
    TA := TGocciaTypedArrayValue(AValue);
    ByteLength := TA.Length * TGocciaTypedArrayValue.BytesPerElement(TA.Kind);
    Data := TA.BufferData;
    Offset := TA.ByteOffset;
  end
  else if AValue is TGocciaDataViewValue then
  begin
    View := TGocciaDataViewValue(AValue);
    Data := ArrayBufferViewBufferData(View.BufferValue);
    Offset := View.ByteOffset;
    if View.IsOutOfBounds then
      ThrowTypeError(SErrorCannotUseDetachedDataView,
        SSuggestArrayBufferDetached);
    if View.IsLengthTracking then
      ByteLength := ArrayBufferViewBufferByteLength(View.BufferValue) - Offset
    else
      ByteLength := View.ByteLengthSlot;
  end
  else
    Exit(False);

  SetLength(ABytes, ByteLength);
  if ByteLength > 0 then
    Move(Data[Offset], ABytes[0], ByteLength);
  Result := True;
end;

function ValueToBytes(const AValue: TGocciaValue; const AMethod: string): TBytes;
var
  Text: string;
begin
  if AValue is TGocciaStringLiteralValue then
  begin
    Text := TGocciaStringLiteralValue(AValue).Value;
    Result := EncodeUTF8WithReplacement(Text);
    Exit;
  end;

  if TryGetArrayBufferViewBytes(AValue, Result) then
    Exit;

  ThrowTypeError(AMethod + ' requires string, TypedArray or DataView data');
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
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be an object');

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

procedure AppendRunCopy(var AOptions: TGocciaSandboxRunOptions;
  const ACopy: TGocciaSandboxCopySpec);
var
  Index: Integer;
begin
  Index := Length(AOptions.Copies);
  SetLength(AOptions.Copies, Index + 1);
  AOptions.Copies[Index] := ACopy;
  AOptions.Isolated := True;
end;

function DecodeBase64Bytes(const AText: string): TBytes;
begin
  if not TryDecodeBase64Standard(AText, Result) then
    raise EConvertError.Create('Invalid base64 data');
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

procedure AddParentPathCopyFromString(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const ASpec: string);
var
  SeparatorIndex: Integer;
  SourcePath: string;
  TargetPath: string;
  CopySpec: TGocciaSandboxCopySpec;
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

  CopySpec.Kind := sckParentPath;
  CopySpec.FromPath := AContext.Fs.Normalize(SourcePath,
    AContext.Shell.WorkingDirectory);
  if TargetPath = '' then
    CopySpec.ToPath := CopySpec.FromPath
  else
    CopySpec.ToPath := AContext.Fs.Normalize(TargetPath, '/');
  CopySpec.ToDirectory := IsDirectoryTargetPath(TargetPath);
  CopySpec.Path := '';
  CopySpec.Text := '';
  CopySpec.Bytes := nil;
  AppendRunCopy(AOptions, CopySpec);
end;

procedure AddRunCopyFromObject(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const AObject: TGocciaObjectValue;
  const AMethod: string);
var
  CopySpec: TGocciaSandboxCopySpec;
  SourceCount: Integer;
begin
  SourceCount := 0;
  if HasDefinedProperty(AObject, 'from') then Inc(SourceCount);
  if HasDefinedProperty(AObject, 'text') then Inc(SourceCount);
  if HasDefinedProperty(AObject, 'base64') then Inc(SourceCount);
  if SourceCount <> 1 then
    ThrowTypeError(AMethod +
      ' copy entries require exactly one of "from", "text", or "base64"');

  CopySpec.FromPath := '';
  CopySpec.ToPath := '';
  CopySpec.ToDirectory := False;
  CopySpec.Path := '';
  CopySpec.Text := '';
  CopySpec.Bytes := nil;

  if HasDefinedProperty(AObject, 'from') then
  begin
    CopySpec.Kind := sckParentPath;
    CopySpec.FromPath := AContext.Fs.Normalize(ObjectStringProperty(AObject,
      'from', AMethod, True), AContext.Shell.WorkingDirectory);
    CopySpec.ToPath := ObjectStringProperty(AObject, 'to', AMethod, False);
    CopySpec.ToDirectory := IsDirectoryTargetPath(CopySpec.ToPath);
    if CopySpec.ToPath = '' then
      CopySpec.ToPath := CopySpec.FromPath
    else
      CopySpec.ToPath := AContext.Fs.Normalize(CopySpec.ToPath, '/');
    AppendRunCopy(AOptions, CopySpec);
    Exit;
  end;

  CopySpec.Path := AContext.Fs.Normalize(ObjectStringProperty(AObject, 'path',
    AMethod, True), '/');
  if HasDefinedProperty(AObject, 'base64') then
  begin
    CopySpec.Kind := sckBytes;
    CopySpec.Bytes := DecodeBase64Bytes(ObjectStringProperty(AObject,
      'base64', AMethod, True));
  end
  else
  begin
    CopySpec.Kind := sckText;
    CopySpec.Text := ObjectStringProperty(AObject, 'text', AMethod, True);
  end;
  AppendRunCopy(AOptions, CopySpec);
end;

procedure AddRunCopyFromValue(const AContext: TGocciaSandboxContext;
  var AOptions: TGocciaSandboxRunOptions; const AValue: TGocciaValue;
  const AMethod: string);
begin
  if AValue is TGocciaStringLiteralValue then
    AddParentPathCopyFromString(AContext, AOptions,
      TGocciaStringLiteralValue(AValue).Value)
  else if AValue is TGocciaObjectValue then
    AddRunCopyFromObject(AContext, AOptions, TGocciaObjectValue(AValue),
      AMethod)
  else
    ThrowTypeError(AMethod + ' copy entries must be strings or objects');
end;

procedure AddRunCopiesFromValue(const AContext: TGocciaSandboxContext;
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
      AddRunCopyFromValue(AContext, AOptions, Arr.GetElement(I), AMethod);
    Exit;
  end;
  AddRunCopyFromValue(AContext, AOptions, AValue, AMethod);
end;

{ The pre-0.14 option names fail with their replacement instead of being
  silently ignored, so a caller written against them does not run without
  the copies or the diff it asked for. }
procedure RejectRemovedRunOption(const AOptionsObject: TGocciaObjectValue;
  const AName, AReplacement, AMethod: string);
var
  Value: TGocciaValue;
begin
  Value := AOptionsObject.GetProperty(AName);
  if Assigned(Value) and not (Value is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(AMethod + ' option "' + AName + '" was removed; ' +
      AReplacement);
end;

const
  { The options runScript reads. A child inherits its parent's capabilities
    and can only narrow them, so there is no option that grants any. }
  RUN_SCRIPT_OPTION_NAMES: array[0..2] of string = ('sandbox', 'copy',
    'diff');

function IsRunScriptOptionName(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(RUN_SCRIPT_OPTION_NAMES) do
    if RUN_SCRIPT_OPTION_NAMES[I] = AName then
      Exit(True);
  Result := False;
end;

function ParseRunScriptOptions(const AContext: TGocciaSandboxContext;
  const AValue: TGocciaValue; const AMethod: string):
  TGocciaSandboxRunOptions;
var
  OptionsObject: TGocciaObjectValue;
  DiffValue: TGocciaValue;
  DiffFormat, Key: string;
begin
  Result := DefaultSandboxRunOptions;
  if (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue) or
     (AValue is TGocciaNullLiteralValue) then
    Exit;
  if not (AValue is TGocciaObjectValue) then
    ThrowTypeError(AMethod + ' options must be an object');

  OptionsObject := TGocciaObjectValue(AValue);
  RejectRemovedRunOption(OptionsObject, 'seed',
    'use "copy" (a path, {from, to}, {path, text}, {path, base64}, or an ' +
    'array of them)', AMethod);
  RejectRemovedRunOption(OptionsObject, 'seeds', 'use "copy"', AMethod);
  RejectRemovedRunOption(OptionsObject, 'diffFormat',
    'use diff: "json" or diff: "unified"', AMethod);
  RejectRemovedRunOption(OptionsObject, 'diffMetadata',
    'JSON diffs always include timestamp metadata; use diff: true',
    AMethod);
  { An option it does not know, such as a capability, is refused rather
    than ignored, so a caller cannot believe it was applied. }
  for Key in OptionsObject.GetOwnPropertyKeys do
    if not IsRunScriptOptionName(Key) then
      ThrowTypeError(AMethod + ' does not accept option "' + Key +
        '" (options: sandbox, copy, diff; a child inherits its parent''s ' +
        'capabilities)');

  if ObjectBooleanProperty(OptionsObject, 'sandbox', AMethod) then
    Result.Isolated := True;

  DiffValue := OptionsObject.GetProperty('diff');
  if Assigned(DiffValue) and not (DiffValue is TGocciaUndefinedLiteralValue) and
     not (DiffValue is TGocciaNullLiteralValue) then
  begin
    if DiffValue is TGocciaBooleanLiteralValue then
    begin
      if TGocciaBooleanLiteralValue(DiffValue).Value then
      begin
        Result.IncludeDiff := True;
        Result.Isolated := True;
      end;
    end
    else if DiffValue is TGocciaStringLiteralValue then
    begin
      DiffFormat := TGocciaStringLiteralValue(DiffValue).Value;
      if (DiffFormat <> SANDBOX_DIFF_FORMAT_JSON) and
         (DiffFormat <> SANDBOX_DIFF_FORMAT_UNIFIED) then
        ThrowTypeError(AMethod +
          ' option "diff" must be true, "json", or "unified"');
      Result.DiffFormat := DiffFormat;
      Result.IncludeDiff := True;
      Result.Isolated := True;
    end
    else
      ThrowTypeError(AMethod +
        ' option "diff" must be true, "json", or "unified"');
  end;

  AddRunCopiesFromValue(AContext, Result, OptionsObject.GetProperty('copy'),
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

{ Search options }

const
  SEARCH_MODULE_NAME = 'goccia:search';
  GLOB_OPTION_NAMES: array[0..1] of string = ('cwd', 'exclude');
  GREP_OPTION_NAMES: array[0..7] of string = ('cwd', 'glob', 'exclude',
    'regex', 'ignoreCase', 'maxResults', 'maxBytes', 'context');
  FUZZY_OPTION_NAMES: array[0..3] of string = ('cwd', 'glob', 'exclude',
    'limit');
  DEFAULT_FUZZY_LIMIT = 100;
  { The largest integer a Number holds exactly. }
  MAX_SAFE_INTEGER = 9007199254740991;

type
  TGocciaSandboxSearchSelection = record
    Cwd: string;
    HasGlob: Boolean;
    Globs: TStringArray;
    Excludes: TStringArray;
  end;

function SearchOptionsObject(const AValue: TGocciaValue;
  const ANames: array of string; const ANameCount: Integer;
  const AMethod: string): TGocciaObjectValue;
var
  Key: string;
  Index: Integer;
  Known: Boolean;
begin
  if IsMissingValue(AValue) then
    Exit(nil);
  if not (AValue is TGocciaObjectValue) or (AValue is TGocciaArrayValue) or
     AValue.IsCallable then
    ThrowTypeError(AMethod + ' options must be an object');
  Result := TGocciaObjectValue(AValue);
  for Key in Result.GetOwnPropertyKeys do
  begin
    Known := False;
    for Index := 0 to ANameCount - 1 do
      if ANames[Index] = Key then
        Known := True;
    if not Known then
      ThrowTypeError(AMethod + ' does not accept option "' + Key + '"');
  end;
end;

{ A pattern argument or option: a string, or an array of strings. }
function SearchPatternList(const AValue: TGocciaValue;
  const AMethod, AName: string): TStringArray;
var
  Arr: TGocciaArrayValue;
  Element: TGocciaValue;
  Index: Integer;
begin
  Result := nil;
  if AValue is TGocciaStringLiteralValue then
  begin
    SetLength(Result, 1);
    Result[0] := TGocciaStringLiteralValue(AValue).Value;
    Exit;
  end;
  if not (AValue is TGocciaArrayValue) then
    ThrowTypeError(AMethod + ' ' + AName +
      ' must be a string or an array of strings');
  Arr := TGocciaArrayValue(AValue);
  SetLength(Result, Arr.GetLength);
  for Index := 0 to Arr.GetLength - 1 do
  begin
    Element := Arr.GetElement(Index);
    if not (Element is TGocciaStringLiteralValue) then
      ThrowTypeError(AMethod + ' ' + AName +
        ' must be a string or an array of strings');
    Result[Index] := TGocciaStringLiteralValue(Element).Value;
  end;
end;

function SearchExcludeList(const AObject: TGocciaObjectValue;
  const AMethod: string): TStringArray;
var
  Value: TGocciaValue;
begin
  Result := nil;
  if not Assigned(AObject) then
    Exit;
  Value := AObject.GetProperty('exclude');
  if IsMissingValue(Value) then
    Exit;
  if not (Value is TGocciaArrayValue) then
    ThrowTypeError(AMethod + ' option "exclude" must be an array of strings');
  Result := SearchPatternList(Value, AMethod, 'option "exclude"');
end;

function SearchCwd(const AObject: TGocciaObjectValue;
  const ADefault, AMethod: string): string;
var
  Value: TGocciaValue;
begin
  Result := ADefault;
  if not Assigned(AObject) then
    Exit;
  Value := AObject.GetProperty('cwd');
  if IsMissingValue(Value) then
    Exit;
  if not (Value is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' option "cwd" must be a string');
  Result := TGocciaStringLiteralValue(Value).Value;
end;

function SearchBooleanOption(const AObject: TGocciaObjectValue;
  const AName, AMethod: string): Boolean;
var
  Value: TGocciaValue;
begin
  Result := False;
  if not Assigned(AObject) then
    Exit;
  Value := AObject.GetProperty(AName);
  if IsMissingValue(Value) then
    Exit;
  if not (Value is TGocciaBooleanLiteralValue) then
    ThrowTypeError(AMethod + ' option "' + AName + '" must be a boolean');
  Result := TGocciaBooleanLiteralValue(Value).Value;
end;

function SearchIntegerOption(const AObject: TGocciaObjectValue;
  const AName, AMethod: string; const ADefault, AMinimum: Int64): Int64;
var
  Value: TGocciaValue;
  Number: Double;
begin
  Result := ADefault;
  if not Assigned(AObject) then
    Exit;
  Value := AObject.GetProperty(AName);
  if IsMissingValue(Value) then
    Exit;
  if not (Value is TGocciaNumberLiteralValue) then
    ThrowTypeError(AMethod + ' option "' + AName + '" must be an integer');
  Number := TGocciaNumberLiteralValue(Value).Value;
  if IsNan(Number) or IsInfinite(Number) or (Frac(Number) <> 0) or
     (Number < AMinimum) or (Number > MAX_SAFE_INTEGER) then
    ThrowTypeError(AMethod + ' option "' + AName +
      '" must be an integer of at least ' + IntToStr(AMinimum));
  Result := Trunc(Number);
end;

function SearchSelection(const AObject: TGocciaObjectValue;
  const ADefaultCwd, AMethod: string): TGocciaSandboxSearchSelection;
var
  Value: TGocciaValue;
begin
  Result := Default(TGocciaSandboxSearchSelection);
  Result.Cwd := SearchCwd(AObject, ADefaultCwd, AMethod);
  Result.Excludes := SearchExcludeList(AObject, AMethod);
  if Assigned(AObject) then
  begin
    Value := AObject.GetProperty('glob');
    if not IsMissingValue(Value) then
    begin
      Result.HasGlob := True;
      Result.Globs := SearchPatternList(Value, AMethod, 'option "glob"');
    end;
  end;
end;

function RequireSearchText(const AArgs: TGocciaArgumentsCollection;
  const AMethod, AName: string): string;
var
  Value: TGocciaValue;
begin
  Value := AArgs.GetElement(0);
  if not (Value is TGocciaStringLiteralValue) then
    ThrowTypeError(AMethod + ' requires a ' + AName + ' string');
  Result := TGocciaStringLiteralValue(Value).Value;
  if Result = '' then
    ThrowTypeError(AMethod + ' requires a non-empty ' + AName);
end;

{ The glob arguments every form shares, validated before any work. }
{ Compiles patterns once, before any search work; a refused pattern is the
  script's TypeError. }
function CompileSearchGlobs(const APatterns: TStringArray;
  const AProgress: TSandboxGlobProgress;
  const AMethod: string): TSandboxGlobPatternArray;
begin
  Result := nil;
  try
    Result := CompileSandboxGlobs(APatterns, AProgress);
  except
    on E: ESandboxGlobError do
      ThrowTypeError(AMethod + ': ' + E.Message);
  end;
end;

procedure ParseGlobArguments(const APattern, AOptions: TGocciaValue;
  const ADefaultCwd, AMethod: string; const AProgress: TSandboxGlobProgress;
  out APatterns, AExcludes: TSandboxGlobPatternArray; out ACwd: string);
var
  Options: TGocciaObjectValue;
  Patterns, Excludes: TStringArray;
begin
  Patterns := SearchPatternList(APattern, AMethod, 'pattern');
  Options := SearchOptionsObject(AOptions, GLOB_OPTION_NAMES,
    Length(GLOB_OPTION_NAMES), AMethod);
  ACwd := SearchCwd(Options, ADefaultCwd, AMethod);
  Excludes := SearchExcludeList(Options, AMethod);
  APatterns := CompileSearchGlobs(Patterns, AProgress, AMethod);
  AExcludes := CompileSearchGlobs(Excludes, AProgress, AMethod);
end;

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
var
  ResultRoot: TGocciaTempRoot;
begin
  { The stdout/stderr strings below are GC safe points (and can be large);
    root the object while it fills. }
  InitializeTempRoot(ResultRoot);
  try
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    4);
  AddTempRootIfNeeded(ResultRoot, Result);
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
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
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
    begin
      { A rejected promise is guest-catchable, so the opaque families must be
        turned away before the conversion below. }
      if IsUncatchableFault(E) then
        raise;
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
    end;
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
    begin
      { A rejected promise is guest-catchable, so the opaque families must be
        turned away before the conversion below. }
      if IsUncatchableFault(E) then
        raise;
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
    end;
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
      Parsed := Parser.Parse(FResult.Output);
    finally
      Parser.Free;
    end;
    Result := Fulfilled(Parsed);
  except
    on E: TGocciaThrowValue do
      Result := RejectedPromise(E.Value);
    on E: Exception do
    begin
      { A rejected promise is guest-catchable, so the opaque families must be
        turned away before the conversion below. }
      if IsUncatchableFault(E) then
        raise;
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
    end;
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

{ TGocciaSandboxGlobIteratorValue }

constructor TGocciaSandboxGlobIteratorValue.Create(
  const AExtension: TGocciaSandboxRuntimeExtension;
  const APatterns, AExcludes: TSandboxGlobPatternArray; const ACwd: string);
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype, 4);
  FExtension := AExtension;
  FPatterns := APatterns;
  FExcludes := AExcludes;
  FCwd := ACwd;
  AddNativeFunction(Self, 'next', Next, 0);
  AddNativeFunction(Self, 'return', ReturnMethod, 0);
  DefineSymbolProperty(TGocciaSymbolValue.WellKnownAsyncIterator,
    TGocciaPropertyDescriptorData.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(AsyncIterator,
        '[Symbol.asyncIterator]', 0), [pfWritable, pfConfigurable]));
end;

function TGocciaSandboxGlobIteratorValue.IteratorResult(
  const AValue: TGocciaValue; const ADone: Boolean): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  ObjRoot: TGocciaTempRoot;
begin
  InitializeTempRoot(ObjRoot);
  try
    AddTempRootIfNeeded(ObjRoot, AValue);
    Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
      2);
    AddTempRootIfNeeded(ObjRoot, Obj);
    Obj.SetProperty('value', AValue);
    Obj.SetProperty('done', TGocciaBooleanLiteralValue.Create(ADone));
    Result := FulfilledPromise(Obj);
  finally
    RemoveTempRootIfNeeded(ObjRoot);
  end;
end;

function TGocciaSandboxGlobIteratorValue.Next(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  try
    if not FStarted then
    begin
      FStarted := True;
      FPaths := FExtension.GlobPaths(FPatterns, FExcludes, FCwd);
      FIndex := 0;
    end;
    if FIndex > High(FPaths) then
      Exit(IteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True));
    Inc(FIndex);
    Result := IteratorResult(TGocciaStringLiteralValue.Create(
      FPaths[FIndex - 1]), False);
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxGlobIteratorValue.ReturnMethod(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FStarted := True;
  FPaths := nil;
  FIndex := 0;
  if IsMissingValue(AArgs.GetElement(0)) then
    Result := IteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True)
  else
    Result := IteratorResult(AArgs.GetElement(0), True);
end;

function TGocciaSandboxGlobIteratorValue.AsyncIterator(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Self;
end;

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
  DateConstructor := GetDateIntrinsic(Runtime.Engine.ShimLoader);
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

procedure TGocciaSandboxRuntimeExtension.HandleRootClamp(
  const APath, ABase, ACanonicalPath: string);
begin
  if Assigned(Runtime) and Assigned(Runtime.Engine) then
    Runtime.Engine.EmitCapabilityAudit(gckSandboxFileSystem, gcdDeny,
      APath, Format('path from %s escaped the sandbox root and was clamped to %s',
        [ABase, ACanonicalPath]));
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
  FFsModule.AddExportValue('glob', FsNamespace.GetProperty('glob'));
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
  FFsModule.AddExportValue('globSync', FsNamespace.GetProperty('globSync'));
  FFsModule.AddExportValue('promises', FsNamespace.GetProperty('promises'));
  if not Runtime.Engine.ModuleLoader.GlobalModules.TryGetValue(
    'fs', FPreviousFsModule) then
    FPreviousFsModule := nil;
  Runtime.Engine.ModuleLoader.GlobalModules.Add('fs', FFsModule);
  FFsModuleRegistered := True;

  FGocciaModule := TGocciaModule.Create('goccia');
  FGocciaModule.AddExportValue(KEYWORD_DEFAULT, GocciaNamespace);
  FGocciaModule.AddExportValue('$', GocciaNamespace.GetProperty('$'));
  FGocciaModule.AddExportValue('runScript',
    GocciaNamespace.GetProperty('runScript'));
  if not Runtime.Engine.ModuleLoader.GlobalModules.TryGetValue(
    'goccia', FPreviousGocciaModule) then
    FPreviousGocciaModule := nil;
  Runtime.Engine.ModuleLoader.GlobalModules.Add('goccia', FGocciaModule);
  FGocciaModuleRegistered := True;

  FSearchModule := TGocciaModule.Create(SEARCH_MODULE_NAME);
  FSearchModule.AddExportValue('grep',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(SearchGrep, 'grep', 2));
  FSearchModule.AddExportValue('fuzzyFind',
    TGocciaNativeFunctionValue.CreateWithoutPrototype(SearchFuzzyFind,
      'fuzzyFind', 2));
  if not Runtime.Engine.ModuleLoader.GlobalModules.TryGetValue(
    SEARCH_MODULE_NAME, FPreviousSearchModule) then
    FPreviousSearchModule := nil;
  Runtime.Engine.ModuleLoader.GlobalModules.Add(SEARCH_MODULE_NAME,
    FSearchModule);
  FSearchModuleRegistered := True;

  FPreviousRootClampCallback := FContext.Fs.RootClampCallback;
  FContext.Fs.RootClampCallback := HandleRootClamp;
  FRootClampInstalled := True;
end;

procedure TGocciaSandboxRuntimeExtension.Detach;
begin
  if FRootClampInstalled then
  begin
    FContext.Fs.RootClampCallback := FPreviousRootClampCallback;
    FPreviousRootClampCallback := nil;
    FRootClampInstalled := False;
  end;
  if Assigned(Runtime) and Assigned(Runtime.Engine) then
  begin
    if FSearchModuleRegistered then
    begin
      if Assigned(FPreviousSearchModule) then
        Runtime.Engine.ModuleLoader.GlobalModules.Add(SEARCH_MODULE_NAME,
          FPreviousSearchModule)
      else
        Runtime.Engine.ModuleLoader.GlobalModules.Remove(SEARCH_MODULE_NAME);
      FPreviousSearchModule := nil;
      FSearchModuleRegistered := False;
    end;
    if FGocciaModuleRegistered then
    begin
      if Assigned(FPreviousGocciaModule) then
        Runtime.Engine.ModuleLoader.GlobalModules.Add(
          'goccia', FPreviousGocciaModule)
      else
        Runtime.Engine.ModuleLoader.GlobalModules.Remove('goccia');
      FPreviousGocciaModule := nil;
      FGocciaModuleRegistered := False;
    end;
    if FFsModuleRegistered then
    begin
      if Assigned(FPreviousFsModule) then
        Runtime.Engine.ModuleLoader.GlobalModules.Add('fs', FPreviousFsModule)
      else
        Runtime.Engine.ModuleLoader.GlobalModules.Remove('fs');
      FPreviousFsModule := nil;
      FFsModuleRegistered := False;
    end;
  end;
  FSearchModule.Free;
  FSearchModule := nil;
  FGocciaModule.Free;
  FGocciaModule := nil;
  FFsModule.Free;
  FFsModule := nil;
  inherited;
end;

procedure TGocciaSandboxRuntimeExtension.SearchProgress;
begin
  CheckNativeWork;
end;

function TGocciaSandboxRuntimeExtension.GlobPaths(const APatterns,
  AExcludes: TSandboxGlobPatternArray; const ACwd: string): TStringArray;
begin
  Result := nil;
  try
    Result := GlobSandbox(FContext.Fs, APatterns, AExcludes,
      FContext.Fs.Normalize(ACwd, FContext.Shell.WorkingDirectory),
      SearchProgress);
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'glob', ACwd));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsGlobSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Patterns, Excludes: TSandboxGlobPatternArray;
  Cwd: string;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.globSync');
  ParseGlobArguments(AArgs.GetElement(0), AArgs.GetElement(1),
    FContext.Shell.WorkingDirectory, 'fs.globSync', SearchProgress, Patterns,
    Excludes, Cwd);
  Result := PathsToArray(GlobPaths(Patterns, Excludes, Cwd));
end;

function TGocciaSandboxRuntimeExtension.FsGlob(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Patterns, Excludes: TSandboxGlobPatternArray;
  Cwd: string;
  Callback, Options: TGocciaValue;
  Queue: TGocciaMicrotaskQueue;
  Job: TGocciaSandboxFsJob;
begin
  ValidateArgumentCount(AArgs, 3, 'fs.glob');
  if AArgs.Length >= 3 then
  begin
    Options := AArgs.GetElement(1);
    Callback := RequireFsCallback(AArgs, 2, 'fs.glob');
  end
  else
  begin
    Options := nil;
    Callback := RequireFsCallback(AArgs, 1, 'fs.glob');
  end;
  ParseGlobArguments(AArgs.GetElement(0), Options,
    FContext.Shell.WorkingDirectory, 'fs.glob', SearchProgress, Patterns,
    Excludes, Cwd);
  Queue := TGocciaMicrotaskQueue.Instance;
  if not Assigned(Queue) then
    raise Exception.Create('Filesystem callbacks require a microtask queue');
  Job := TGocciaSandboxFsJob.Create(Self, sfoGlob, Cwd, '', nil, False,
    False, sfcValue, Callback, nil);
  Job.FGlobPatterns := Patterns;
  Job.FGlobExcludes := Excludes;
  Queue.EnqueueJob(Job);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesGlob(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Patterns, Excludes: TSandboxGlobPatternArray;
  Cwd: string;
begin
  ValidateArgumentCount(AArgs, 2, 'fs.promises.glob');
  ParseGlobArguments(AArgs.GetElement(0), AArgs.GetElement(1),
    FContext.Shell.WorkingDirectory, 'fs.promises.glob', SearchProgress,
    Patterns, Excludes, Cwd);
  Result := TGocciaSandboxGlobIteratorValue.Create(Self, Patterns, Excludes,
    Cwd);
end;

function SearchFilesOrThrow(const AContext: TGocciaSandboxContext;
  const ASelection: TGocciaSandboxSearchSelection;
  const AProgress: TSandboxGlobProgress;
  const AMethod: string): TSandboxSearchFileArray;
var
  Globs, Excludes: TSandboxGlobPatternArray;
begin
  Result := nil;
  Globs := CompileSearchGlobs(ASelection.Globs, AProgress, AMethod);
  Excludes := CompileSearchGlobs(ASelection.Excludes, AProgress, AMethod);
  try
    Result := SandboxSearchFiles(AContext.Fs,
      AContext.Fs.Normalize(ASelection.Cwd, AContext.Shell.WorkingDirectory),
      ASelection.HasGlob, Globs, Excludes, AProgress);
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'search', ASelection.Cwd));
  end;
end;

{ AObject, which the caller keeps rooted, gets the strings as an array
  property; the array is attached before its strings are allocated. }
procedure SetStringsProperty(const AObject: TGocciaObjectValue;
  const AName: string; const AStrings: TStringArray);
var
  Arr: TGocciaArrayValue;
  Index: Integer;
begin
  Arr := TGocciaArrayValue.Create(nil, Length(AStrings));
  AObject.SetProperty(AName, Arr);
  for Index := 0 to High(AStrings) do
    ArrayCreateDataProperty(Arr, Index,
      TGocciaStringLiteralValue.Create(AStrings[Index]));
end;

function TGocciaSandboxRuntimeExtension.SearchGrep(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Options: TGocciaObjectValue;
  Selection: TGocciaSandboxSearchSelection;
  GrepOptions: TGocciaSandboxGrepOptions;
  Files: TSandboxSearchFileArray;
  Found: TGocciaSandboxGrepResult;
  Obj, MatchObj: TGocciaObjectValue;
  Matches: TGocciaArrayValue;
  ObjRoot: TGocciaTempRoot;
  Index: Integer;
begin
  ValidateArgumentCount(AArgs, 2, 'grep');
  GrepOptions := Default(TGocciaSandboxGrepOptions);
  GrepOptions.Pattern := RequireSearchText(AArgs, 'grep', 'pattern');
  Options := SearchOptionsObject(AArgs.GetElement(1), GREP_OPTION_NAMES,
    Length(GREP_OPTION_NAMES), 'grep');
  Selection := SearchSelection(Options, FContext.Shell.WorkingDirectory,
    'grep');
  GrepOptions.Regex := SearchBooleanOption(Options, 'regex', 'grep');
  GrepOptions.IgnoreCase := SearchBooleanOption(Options, 'ignoreCase', 'grep');
  { Counts past what a search can hold mean "no limit": clamp them so the
    native arithmetic stays in range. }
  GrepOptions.MaxResults := Integer(Min(SearchIntegerOption(Options,
    'maxResults', 'grep', DEFAULT_SANDBOX_GREP_MAX_RESULTS, 1), MaxInt));
  GrepOptions.MaxBytes := SearchIntegerOption(Options, 'maxBytes', 'grep',
    SANDBOX_GREP_NO_BYTE_LIMIT, 0);
  GrepOptions.Context := Integer(Min(SearchIntegerOption(Options, 'context',
    'grep', 0, 0), MAX_SANDBOX_GREP_CONTEXT));
  Files := SearchFilesOrThrow(FContext, Selection, SearchProgress, 'grep');
  Found := SandboxGrep(FContext.Fs, Files, GrepOptions);
  InitializeTempRoot(ObjRoot);
  try
    Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
      2);
    AddTempRootIfNeeded(ObjRoot, Obj);
    Matches := TGocciaArrayValue.Create(nil, Length(Found.Matches));
    Obj.SetProperty('matches', Matches);
    Obj.SetProperty('truncated',
      TGocciaBooleanLiteralValue.Create(Found.Truncated));
    for Index := 0 to High(Found.Matches) do
    begin
      MatchObj := TGocciaObjectValue.Create(
        TGocciaObjectValue.SharedObjectPrototype, 6);
      ArrayCreateDataProperty(Matches, Index, MatchObj);
      MatchObj.SetProperty('path',
        TGocciaStringLiteralValue.Create(Found.Matches[Index].Path));
      MatchObj.SetProperty('line',
        TGocciaNumberLiteralValue.Create(Found.Matches[Index].Line));
      MatchObj.SetProperty('column',
        TGocciaNumberLiteralValue.Create(Found.Matches[Index].Column));
      MatchObj.SetProperty('text',
        TGocciaStringLiteralValue.Create(Found.Matches[Index].Text));
      if GrepOptions.Context > 0 then
      begin
        SetStringsProperty(MatchObj, 'before', Found.Matches[Index].Before);
        SetStringsProperty(MatchObj, 'after', Found.Matches[Index].After);
      end;
    end;
    Result := Obj;
  finally
    RemoveTempRootIfNeeded(ObjRoot);
    Found.Matches := nil;
    ReleaseNativeBytes(Found.ReservedBytes);
  end;
end;

function TGocciaSandboxRuntimeExtension.SearchFuzzyFind(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Query: string;
  Options: TGocciaObjectValue;
  Selection: TGocciaSandboxSearchSelection;
  Limit: Int64;
  Files: TSandboxSearchFileArray;
  Found: TSandboxFuzzyMatchArray;
  Match: TSandboxFuzzyMatch;
  Count, Index, PositionIndex: Integer;
  Obj, MatchObj: TGocciaObjectValue;
  Matches, Positions: TGocciaArrayValue;
  ObjRoot: TGocciaTempRoot;
begin
  ValidateArgumentCount(AArgs, 2, 'fuzzyFind');
  Query := RequireSearchText(AArgs, 'fuzzyFind', 'query');
  Options := SearchOptionsObject(AArgs.GetElement(1), FUZZY_OPTION_NAMES,
    Length(FUZZY_OPTION_NAMES), 'fuzzyFind');
  Selection := SearchSelection(Options, FContext.Shell.WorkingDirectory,
    'fuzzyFind');
  Limit := Min(SearchIntegerOption(Options, 'limit', 'fuzzyFind',
    DEFAULT_FUZZY_LIMIT, 1), MaxInt);
  Files := SearchFilesOrThrow(FContext, Selection, SearchProgress,
    'fuzzyFind');
  Query := FoldSandboxFuzzyQuery(Query);
  Found := nil;
  SetLength(Found, Length(Files));
  Count := 0;
  for Index := 0 to High(Files) do
  begin
    CheckNativeWork;
    if TrySandboxFuzzyMatch(Files[Index].Path, Query, Match) then
    begin
      Found[Count] := Match;
      Inc(Count);
    end;
  end;
  SetLength(Found, Count);
  SortSandboxFuzzyMatches(Found);
  if Count > Limit then
    Count := Integer(Limit);
  InitializeTempRoot(ObjRoot);
  try
    Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
      2);
    AddTempRootIfNeeded(ObjRoot, Obj);
    Matches := TGocciaArrayValue.Create(nil, Count);
    Obj.SetProperty('matches', Matches);
    Obj.SetProperty('truncated',
      TGocciaBooleanLiteralValue.Create(Length(Found) > Limit));
    for Index := 0 to Count - 1 do
    begin
      MatchObj := TGocciaObjectValue.Create(
        TGocciaObjectValue.SharedObjectPrototype, 3);
      ArrayCreateDataProperty(Matches, Index, MatchObj);
      MatchObj.SetProperty('path',
        TGocciaStringLiteralValue.Create(Found[Index].Path));
      MatchObj.SetProperty('score',
        TGocciaNumberLiteralValue.Create(Found[Index].Score));
      Positions := TGocciaArrayValue.Create(nil,
        Length(Found[Index].Positions));
      MatchObj.SetProperty('positions', Positions);
      for PositionIndex := 0 to High(Found[Index].Positions) do
        ArrayCreateDataProperty(Positions, PositionIndex, TGocciaNumberLiteralValue.Create(
          Found[Index].Positions[PositionIndex]));
    end;
    Result := Obj;
  finally
    RemoveTempRootIfNeeded(ObjRoot);
  end;
end;

function TGocciaSandboxRuntimeExtension.CreateFsNamespace: TGocciaObjectValue;
var
  Promises: TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    32);
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
  AddNativeFunction(Result, 'globSync', FsGlobSync, 2);

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
  AddNativeFunction(Result, 'glob', FsGlob, 3);

  Promises := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    13);
  AddNativeFunction(Promises, 'readFile', FsPromisesReadFile, 2);
  AddNativeFunction(Promises, 'writeFile', FsPromisesWriteFile, 3);
  AddNativeFunction(Promises, 'appendFile', FsPromisesAppendFile, 3);
  AddNativeFunction(Promises, 'mkdir', FsPromisesMkdir, 2);
  AddNativeFunction(Promises, 'readdir', FsPromisesReaddir, 2);
  AddNativeFunction(Promises, 'stat', FsPromisesStat, 1);
  AddNativeFunction(Promises, 'rm', FsPromisesRm, 2);
  AddNativeFunction(Promises, 'rename', FsPromisesRename, 2);
  AddNativeFunction(Promises, 'copyFile', FsPromisesCopyFile, 3);
  AddNativeFunction(Promises, 'glob', FsPromisesGlob, 2);
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
  ObjRoot: TGocciaTempRoot;
begin
  EntryPath := RequireStringArg(AArgs, 0, 'runScript');
  if not Assigned(FContext.RunScriptCallback) then
    ThrowTypeError('runScript is not configured');

  Options := ParseRunScriptOptions(FContext, AArgs.GetElement(1), 'runScript');
  RunResult := FContext.RunScriptCallback(FContext, FContext.Fs.Normalize(
    EntryPath, FContext.Shell.WorkingDirectory), Options);
  { The stdout/stderr/diff strings below are GC safe points (and can be
    large); root the result object while it fills. }
  InitializeTempRoot(ObjRoot);
  try
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype, 8);
  AddTempRootIfNeeded(ObjRoot, Obj);
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
  { Why the run ended, next to the message that says it in prose.  A
    caller orchestrating nested runs has to tell "the child is buggy"
    from "the child hit a ceiling I set" to decide whether retrying or
    raising the ceiling is the answer, and `error` alone forces that
    decision to be made by matching on message text. }
  Obj.SetProperty('failureKind', TGocciaStringLiteralValue.Create(
    SandboxFailureKindName(RunResult.FailureKind)));
  Result := Obj;
  finally
    RemoveTempRootIfNeeded(ObjRoot);
  end;
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
    { Opening for append refuses a read-only path before the existing
      content is copied, and creates a missing file. An append writes; it
      does not read, so it leaves the access time alone, and one refused
      for a read-only path changes nothing at all. }
    FContext.Fs.Open(APath, omAppend).Free;
    Existing := FContext.Fs.SnapshotReadAllBytes(APath);
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
  ArrRoot: TGocciaTempRoot;
  I: Integer;
begin
  try
    Entries := FContext.Fs.List(APath);
    Arr := TGocciaArrayValue.Create(nil, Length(Entries));
    // Only this frame holds the array while its entries are created, and
    // creating an element can collect at the growth gate.
    InitializeTempRoot(ArrRoot);
    try
      AddTempRootIfNeeded(ArrRoot, Arr);
      for I := 0 to High(Entries) do
        ArrayCreateDataProperty(Arr, I,
          TGocciaStringLiteralValue.Create(Entries[I].Name));
    finally
      RemoveTempRootIfNeeded(ArrRoot);
    end;
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
  try
    ValidateArgumentCount(AArgs, 2, 'fs.promises.readFile');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.readFile');
    OptionsValue := AArgs.GetElement(1);
    TextResult := ParseReadOptions(OptionsValue, 'fs.promises.readFile');
    Result := EnqueueFsPromise(Self, sfoReadFile, Path, '', nil,
      TextResult, False, sfcValue, 'fs.promises.readFile');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesWriteFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Data: TBytes;
begin
  try
    ValidateArgumentCount(AArgs, 3, 'fs.promises.writeFile');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.writeFile');
    Data := RequireAsyncData(AArgs, 1, 'fs.promises.writeFile');
    ValidateNoOptions(AArgs.GetElement(2), 'fs.promises.writeFile');
    Result := EnqueueFsPromise(Self, sfoWriteFile, Path, '', Data,
      False, False, sfcErrorOnly, 'fs.promises.writeFile');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesAppendFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Data: TBytes;
begin
  try
    ValidateArgumentCount(AArgs, 3, 'fs.promises.appendFile');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.appendFile');
    Data := RequireAsyncData(AArgs, 1, 'fs.promises.appendFile');
    ValidateNoOptions(AArgs.GetElement(2), 'fs.promises.appendFile');
    Result := EnqueueFsPromise(Self, sfoAppendFile, Path, '', Data,
      False, False, sfcErrorOnly, 'fs.promises.appendFile');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesMkdir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Recursive: Boolean;
begin
  try
    ValidateArgumentCount(AArgs, 2, 'fs.promises.mkdir');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.mkdir');
    OptionsValue := AArgs.GetElement(1);
    Recursive := ValidateRecursiveOptions(OptionsValue, 'fs.promises.mkdir');
    Result := EnqueueFsPromise(Self, sfoMkdir, Path, '', nil, False,
      Recursive, sfcMkdir, 'fs.promises.mkdir');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesReaddir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  try
    ValidateArgumentCount(AArgs, 2, 'fs.promises.readdir');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.readdir');
    ValidateNoOptions(AArgs.GetElement(1), 'fs.promises.readdir');
    Result := EnqueueFsPromise(Self, sfoReaddir, Path, '', nil, False,
      False, sfcValue, 'fs.promises.readdir');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesStat(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  try
    ValidateArgumentCount(AArgs, 2, 'fs.promises.stat');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.stat');
    ValidateNoOptions(AArgs.GetElement(1), 'fs.promises.stat');
    Result := EnqueueFsPromise(Self, sfoStat, Path, '', nil, False,
      False, sfcValue, 'fs.promises.stat');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesRm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  OptionsValue: TGocciaValue;
  Recursive: Boolean;
begin
  try
    ValidateArgumentCount(AArgs, 2, 'fs.promises.rm');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.rm');
    OptionsValue := AArgs.GetElement(1);
    Recursive := ValidateRecursiveOptions(OptionsValue, 'fs.promises.rm');
    Result := EnqueueFsPromise(Self, sfoRm, Path, '', nil, False,
      Recursive, sfcErrorOnly, 'fs.promises.rm');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesRename(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
begin
  try
    ValidateArgumentCount(AArgs, 2, 'fs.promises.rename');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.rename');
    Destination := RequireAsyncPath(AArgs, 1, 'fs.promises.rename');
    Result := EnqueueFsPromise(Self, sfoRename, Path, Destination, nil,
      False, False, sfcErrorOnly, 'fs.promises.rename');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

function TGocciaSandboxRuntimeExtension.FsPromisesCopyFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Destination: string;
begin
  try
    ValidateArgumentCount(AArgs, 3, 'fs.promises.copyFile');
    Path := RequireAsyncPath(AArgs, 0, 'fs.promises.copyFile');
    Destination := RequireAsyncPath(AArgs, 1, 'fs.promises.copyFile');
    ValidateCopyMode(AArgs.GetElement(2), 'fs.promises.copyFile');
    Result := EnqueueFsPromise(Self, sfoCopyFile, Path, Destination, nil,
      False, False, sfcErrorOnly, 'fs.promises.copyFile');
  except
    on E: Exception do
      Result := RejectedPromiseFromException(E);
  end;
end;

initialization
  // A stats object is ordinary; its fields only back the prototype methods.
  RegisterOrdinaryObjectClass(TGocciaSandboxStatValue);
  GSandboxStatsPrototypeSlot :=
    RegisterRealmSlot('Sandbox.fs.Stats.prototype');

end.
