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

  SandboxShell,
  SandboxVirtualFileSystem,

  Goccia.JSON,
  Goccia.Keywords.Reserved,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.PromiseValue,
  Goccia.Values.TypedArrayValue;

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
  SetProperty('mtimeMs', TGocciaNumberLiteralValue.Create(AStat.ModifiedAt));
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
  Result.SetProperty('stdout',
    TGocciaStringLiteralValue.Create(FResult.Output));
  Result.SetProperty('stderr',
    TGocciaStringLiteralValue.Create(FResult.ErrorOutput));
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
  AddNativeFunction(Result, 'runScript', RunScript, 1);
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
  RunResult: TGocciaSandboxRunResult;
  Obj: TGocciaObjectValue;
begin
  EntryPath := RequireStringArg(AArgs, 0, 'runScript');
  if not Assigned(FContext.RunScriptCallback) then
    ThrowTypeError('runScript is not configured');

  RunResult := FContext.RunScriptCallback(FContext.Fs.Normalize(EntryPath,
    FContext.Shell.WorkingDirectory));
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype, 6);
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
  if WantsTextResult(AArgs.GetElement(1)) then
    Exit(TGocciaStringLiteralValue.Create(FContext.Fs.ReadAllText(Path)));
  Bytes := FContext.Fs.ReadAllBytes(Path);
  Result := BytesToUint8Array(Bytes);
end;

function TGocciaSandboxRuntimeExtension.FsWriteFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FContext.Fs.WriteAllBytes(RequireStringArg(AArgs, 0, 'fs.writeFileSync'),
    ValueToBytes(AArgs.GetElement(1), 'fs.writeFileSync'));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSandboxRuntimeExtension.FsAppendFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Existing: TBytes;
  Added: TBytes;
  Combined: TBytes;
begin
  Existing := nil;
  if FContext.Fs.Exists(RequireStringArg(AArgs, 0, 'fs.appendFileSync')) then
    Existing := FContext.Fs.ReadAllBytes(RequireStringArg(AArgs, 0,
      'fs.appendFileSync'));
  Added := ValueToBytes(AArgs.GetElement(1), 'fs.appendFileSync');
  SetLength(Combined, Length(Existing) + Length(Added));
  if Length(Existing) > 0 then
    Move(Existing[0], Combined[0], Length(Existing));
  if Length(Added) > 0 then
    Move(Added[0], Combined[Length(Existing)], Length(Added));
  FContext.Fs.WriteAllBytes(RequireStringArg(AArgs, 0, 'fs.appendFileSync'),
    Combined);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSandboxRuntimeExtension.FsMkdirSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FContext.Fs.MakeDirectory(RequireStringArg(AArgs, 0, 'fs.mkdirSync'),
    OptionRecursive(AArgs.GetElement(1)));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSandboxRuntimeExtension.FsReaddirSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Entries: TSandboxFsStatArray;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Entries := FContext.Fs.List(RequireStringArg(AArgs, 0, 'fs.readdirSync'));
  Arr := TGocciaArrayValue.Create(nil, Length(Entries));
  for I := 0 to High(Entries) do
    Arr.SetElement(I, TGocciaStringLiteralValue.Create(Entries[I].Name));
  Result := Arr;
end;

function TGocciaSandboxRuntimeExtension.FsStatSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaSandboxStatValue.Create(
    FContext.Fs.Stat(RequireStringArg(AArgs, 0, 'fs.statSync')));
end;

function TGocciaSandboxRuntimeExtension.FsExistsSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(
    FContext.Fs.Exists(RequireStringArg(AArgs, 0, 'fs.existsSync')));
end;

function TGocciaSandboxRuntimeExtension.FsRmSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FContext.Fs.DeletePath(RequireStringArg(AArgs, 0, 'fs.rmSync'),
    OptionRecursive(AArgs.GetElement(1)));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSandboxRuntimeExtension.FsRenameSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FContext.Fs.MovePath(RequireStringArg(AArgs, 0, 'fs.renameSync'),
    RequireStringArg(AArgs, 1, 'fs.renameSync'));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaSandboxRuntimeExtension.FsCopyFileSync(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FContext.Fs.CopyPath(RequireStringArg(AArgs, 0, 'fs.copyFileSync'),
    RequireStringArg(AArgs, 1, 'fs.copyFileSync'), False);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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
