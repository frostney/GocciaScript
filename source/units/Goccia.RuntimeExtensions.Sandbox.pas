unit Goccia.RuntimeExtensions.Sandbox;

{$I Goccia.inc}

interface

uses
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
    function FsRm(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsRename(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FsCopyFile(const AArgs: TGocciaArgumentsCollection;
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
  SysUtils,

  base64,
  SandboxShell,
  SandboxVirtualFileSystem,

  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.Sandbox.FileSystemErrors,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.PromiseValue,
  Goccia.Values.TypedArrayValue;

const
  MILLISECONDS_PER_DAY = 24 * 60 * 60 * 1000;

type
  TGocciaSandboxStatValue = class(TGocciaObjectValue)
  private
    FKind: TSandboxFsNodeKind;
  public
    constructor Create(const AStat: TSandboxFsStat);
    function IsFile(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function IsDirectory(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
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

function DateTimeToUnixMilliseconds(const AValue: TDateTime): Double;
begin
  Result := (AValue - EncodeDate(1970, 1, 1)) * MILLISECONDS_PER_DAY;
end;

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

function RunValueAsPromise(const ACallback: TGocciaNativeFunctionCallback;
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue):
  TGocciaValue;
begin
  try
    Result := FulfilledPromise(ACallback(AArgs, AThisValue));
  except
    on E: TGocciaThrowValue do
      Result := RejectedPromise(E.Value);
    on E: Exception do
      Result := RejectedPromise(TGocciaStringLiteralValue.Create(E.Message));
  end;
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

constructor TGocciaSandboxStatValue.Create(const AStat: TSandboxFsStat);
begin
  inherited Create(TGocciaObjectValue.SharedObjectPrototype, 8);
  FKind := AStat.Kind;
  SetProperty('path', TGocciaStringLiteralValue.Create(AStat.Path));
  SetProperty('name', TGocciaStringLiteralValue.Create(AStat.Name));
  SetProperty('type', TGocciaStringLiteralValue.Create(
    SandboxFsKindName(AStat.Kind)));
  SetProperty('size', TGocciaNumberLiteralValue.Create(AStat.Size));
  SetProperty('mtimeMs',
    TGocciaNumberLiteralValue.Create(DateTimeToUnixMilliseconds(
      AStat.ModifiedAt)));
  AddNativeFunction(Self, 'isFile', IsFile, 0);
  AddNativeFunction(Self, 'isDirectory', IsDirectory, 0);
end;

function TGocciaSandboxStatValue.IsFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(FKind = nkFile);
end;

function TGocciaSandboxStatValue.IsDirectory(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(FKind = nkDirectory);
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
    16);
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

  Promises := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype,
    12);
  AddNativeFunction(Promises, 'readFile', FsReadFile, 2);
  AddNativeFunction(Promises, 'writeFile', FsWriteFile, 2);
  AddNativeFunction(Promises, 'appendFile', FsAppendFile, 2);
  AddNativeFunction(Promises, 'mkdir', FsMkdir, 2);
  AddNativeFunction(Promises, 'readdir', FsReaddir, 1);
  AddNativeFunction(Promises, 'stat', FsStat, 1);
  AddNativeFunction(Promises, 'rm', FsRm, 2);
  AddNativeFunction(Promises, 'rename', FsRename, 2);
  AddNativeFunction(Promises, 'copyFile', FsCopyFile, 2);
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
  Bytes: TBytes;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.readFileSync');
  try
    if WantsTextResult(AArgs.GetElement(1)) then
      Exit(TGocciaStringLiteralValue.Create(FContext.Fs.ReadAllText(Path)));
    Bytes := FContext.Fs.ReadAllBytes(Path);
    Result := BytesToUint8Array(Bytes);
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'readFile', Path));
  end;
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
  try
    FContext.Fs.WriteAllBytes(Path, Bytes);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'writeFile', Path));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsAppendFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Existing: TBytes;
  Added: TBytes;
  Combined: TBytes;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.appendFileSync');
  Existing := nil;
  try
    if FContext.Fs.Exists(Path) then
      Existing := FContext.Fs.ReadAllBytes(Path);
    Added := ValueToBytes(AArgs.GetElement(1), 'fs.appendFileSync');
    SetLength(Combined, Length(Existing) + Length(Added));
    if Length(Existing) > 0 then
      Move(Existing[0], Combined[0], Length(Existing));
    if Length(Added) > 0 then
      Move(Added[0], Combined[Length(Existing)], Length(Added));
    FContext.Fs.WriteAllBytes(Path, Combined);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'appendFile', Path));
  end;
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
  try
    FContext.Fs.MakeDirectory(Path, Recursive);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'mkdir', Path));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsReaddirSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
  Entries: TSandboxFsStatArray;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.readdirSync');
  try
    Entries := FContext.Fs.List(Path);
    Arr := TGocciaArrayValue.Create(nil, Length(Entries));
    for I := 0 to High(Entries) do
      Arr.SetElement(I, TGocciaStringLiteralValue.Create(Entries[I].Name));
    Result := Arr;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'readdir', Path));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsStatSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.statSync');
  try
    Result := TGocciaSandboxStatValue.Create(FContext.Fs.Stat(Path));
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'stat', Path));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsExistsSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Path: string;
begin
  Path := RequireStringArg(AArgs, 0, 'fs.existsSync');
  try
    Result := TGocciaBooleanLiteralValue.Create(FContext.Fs.Exists(Path));
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'exists', Path));
  end;
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
  try
    FContext.Fs.DeletePath(Path, Recursive);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(
        CreateSandboxFileSystemError(E, 'rm', Path));
  end;
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
  try
    FContext.Fs.MovePath(Path, Destination);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(CreateSandboxFileSystemError(E,
        'rename', Path, Destination));
  end;
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
  try
    FContext.Fs.CopyPath(Path, Destination, False);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  except
    on E: ESandboxFsError do
      raise TGocciaThrowValue.Create(CreateSandboxFileSystemError(E,
        'copyFile', Path, Destination));
  end;
end;

function TGocciaSandboxRuntimeExtension.FsReadFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsReadFileSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsWriteFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsWriteFileSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsAppendFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsAppendFileSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsMkdir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsMkdirSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsReaddir(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsReaddirSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsStat(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsStatSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsRm(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsRmSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsRename(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsRenameSync, AArgs, AThisValue);
end;

function TGocciaSandboxRuntimeExtension.FsCopyFile(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := RunValueAsPromise(FsCopyFileSync, AArgs, AThisValue);
end;

end.
