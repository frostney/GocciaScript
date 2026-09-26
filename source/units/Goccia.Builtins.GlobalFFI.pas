unit Goccia.Builtins.GlobalFFI;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Capabilities,
  Goccia.CapabilityAudit,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalFFI = class(TGocciaBuiltin)
  private
    FCapabilities: TGocciaCapabilities;
    FCapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
  published
    function FFIOpen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIStruct(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIUnion(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFICallback(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFINullable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIVarArgs(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIMetadata(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFINullptrGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFISuffixGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    { FFI.open checks every library path against ACapabilities' ffi scopes
      (ADR 0122). }
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const ACapabilities: TGocciaCapabilities;
      const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter);
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FFI.DynamicLibrary,
  Goccia.FFI.Types,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFILibrary,
  Goccia.Values.FFIPointer,
  Goccia.Values.FFIType,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

const
  {$IFDEF DARWIN}
  SHARED_LIBRARY_SUFFIX = '.dylib';
  {$ELSE}
  {$IFDEF MSWINDOWS}
  SHARED_LIBRARY_SUFFIX = '.dll';
  {$ELSE}
  SHARED_LIBRARY_SUFFIX = '.so';
  {$ENDIF}
  {$ENDIF}

constructor TGocciaGlobalFFI.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback;
  const ACapabilities: TGocciaCapabilities;
  const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);
  FCapabilities := ACapabilities;
  FCapabilityAuditEmitter := ACapabilityAuditEmitter;

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod(PROP_OPEN, FFIOpen, 1, gmkStaticMethod);
    Members.AddNamedMethod('struct', FFIStruct, 1, gmkStaticMethod);
    Members.AddNamedMethod('union', FFIUnion, 1, gmkStaticMethod);
    Members.AddNamedMethod('array', FFIArray, 2, gmkStaticMethod);
    Members.AddNamedMethod('callback', FFICallback, 1, gmkStaticMethod);
    Members.AddNamedMethod('nullable', FFINullable, 1, gmkStaticMethod);
    Members.AddNamedMethod('varargs', FFIVarArgs, 2, gmkStaticMethod);
    Members.AddNamedMethod('metadata', FFIMetadata, 1, gmkStaticMethod);
    Members.AddAccessor(PROP_NULLPTR, FFINullptrGetter, nil, [pfConfigurable], gmkStaticGetter);
    Members.AddAccessor(PROP_SUFFIX, FFISuffixGetter, nil, [pfConfigurable], gmkStaticGetter);
    RegisterMemberDefinitions(FBuiltinObject, Members.ToDefinitions);
  finally
    Members.Free;
  end;

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtConst, True);
end;

function TGocciaGlobalFFI.FFIStruct(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFIStructRequiresDefinition,
      SSuggestFFIUsage);
  Result := CreateFFIStructType(AArgs.GetElement(0));
end;

function TGocciaGlobalFFI.FFIUnion(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFIUnionRequiresDefinition,
      SSuggestFFIUsage);
  Result := CreateFFIUnionType(AArgs.GetElement(0));
end;

function TGocciaGlobalFFI.FFIArray(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 2 then
    ThrowTypeError(SErrorFFIArrayRequiresTypeAndLength,
      SSuggestFFIUsage);
  Result := CreateFFIArrayType(AArgs.GetElement(0), AArgs.GetElement(1));
end;

function TGocciaGlobalFFI.FFICallback(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFICallbackRequiresDefinition,
      SSuggestFFIUsage);
  Result := CreateFFICallbackType(AArgs.GetElement(0));
end;

function TGocciaGlobalFFI.FFINullable(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFINullableRequiresType, SSuggestFFIUsage);
  Result := CreateFFINullableType(AArgs.GetElement(0));
end;

function TGocciaGlobalFFI.FFIVarArgs(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length < 2 then
    ThrowTypeError(SErrorFFIVarArgsRequiresArrays, SSuggestFFIUsage);
  Result := CreateFFIVarArgs(AArgs.GetElement(0), AArgs.GetElement(1));
end;

function TGocciaGlobalFFI.FFIMetadata(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Aggregate: TGocciaFFIAggregateValue;
  Metadata: TGocciaObjectValue;
begin
  if (AArgs.Length < 1) or
     not (AArgs.GetElement(0) is TGocciaFFIAggregateValue) then
    ThrowTypeError(SErrorFFIMetadataRequiresAggregate, SSuggestFFIUsage);
  Aggregate := TGocciaFFIAggregateValue(AArgs.GetElement(0));
  Aggregate.EnsureBackingStore;
  Metadata := TGocciaObjectValue.Create;
  Metadata.CreateDataPropertyOrThrow('buffer', Aggregate.Buffer);
  Metadata.CreateDataPropertyOrThrow('byteOffset',
    TGocciaNumberLiteralValue.Create(Aggregate.ByteOffset));
  Metadata.CreateDataPropertyOrThrow('size',
    TGocciaNumberLiteralValue.Create(Aggregate.Descriptor.Size));
  if Aggregate.Descriptor.Kind = ftkArray then
    Metadata.CreateDataPropertyOrThrow('length',
      TGocciaNumberLiteralValue.Create(Aggregate.Descriptor.ElementCount));
  Result := Metadata;
end;

{ True when APath names a library without any directory part. }
function IsBareLibraryName(const APath: string): Boolean;
begin
  Result := (Pos('/', APath) = 0) {$IFDEF MSWINDOWS} and (Pos('\', APath) = 0) and
    (Pos(':', APath) = 0){$ENDIF};
end;

function TGocciaGlobalFFI.FFIOpen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LibPath, LoadPath, DenialDetail: string;
  Allowed: Boolean;
  Handle: TGocciaFFILibraryHandle;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFIOpenRequiresPath, SSuggestFFILibraryOpen);

  LibPath := AArgs.GetElement(0).ToStringLiteral.Value;
  { A name with no directory part is found by the platform loader's search
    path, which no path scope describes, so only an unscoped grant with no
    deny scope covers it. Anything else is judged, and then loaded, at its
    canonical path, so the file checked is the file opened. }
  if IsBareLibraryName(LibPath) then
  begin
    LoadPath := LibPath;
    Allowed := FCapabilities.AllowsUnscoped(gcFFI);
    DenialDetail := SSuggestFFIBareName;
  end
  else
  begin
    LoadPath := CanonicalCapabilityPath(LibPath);
    Allowed := FCapabilities.AllowsPath(gcFFI, LoadPath);
    if FCapabilities.DeniesPath(gcFFI, LoadPath) then
      DenialDetail := Format(SSuggestFFIDenied, [LoadPath])
    else
      DenialDetail := Format(SSuggestFFINotGranted, [LoadPath,
        ExtractFileDir(LoadPath)]);
  end;
  if not Allowed then
  begin
    if Assigned(FCapabilityAuditEmitter) then
      FCapabilityAuditEmitter(gckFFIOpen, gcdDeny, LibPath,
        'the ffi capability does not cover this library');
    ThrowPermissionDenied(CapabilityName(gcFFI), LibPath, DenialDetail);
  end;
  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFFIOpen, gcdAllow, LibPath,
      'the ffi capability covers this library');

  try
    Handle := TGocciaFFILibraryHandle.Create(LibPath, LoadPath);
  except
    on E: Exception do
      ThrowTypeError(E.Message, SSuggestFFILibraryOpen);
  end;

  try
    Result := TGocciaFFILibraryValue.Create(Handle);
  except
    Handle.ReleaseOwner;
    raise;
  end;
end;

function TGocciaGlobalFFI.FFINullptrGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaFFIPointerValue.NullPointer;
end;

function TGocciaGlobalFFI.FFISuffixGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(SHARED_LIBRARY_SUFFIX);
end;

end.
