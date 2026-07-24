unit Goccia.Builtins.GlobalFFI;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.CapabilityAudit,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalFFI = class(TGocciaBuiltin)
  private
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
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
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
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFILibrary,
  Goccia.Values.FFIPointer,
  Goccia.Values.FFIType,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FStaticMembers, 0);
end;

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
  const ACapabilityAuditEmitter: TGocciaCapabilityAuditEmitter);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);
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
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

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

function TGocciaGlobalFFI.FFIOpen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LibPath: string;
  Handle: TGocciaFFILibraryHandle;
begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFIOpenRequiresPath, SSuggestFFILibraryOpen);

  LibPath := AArgs.GetElement(0).ToStringLiteral.Value;
  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFFIOpen, gcdAllow, LibPath,
      'FFI capability is enabled');

  try
    Handle := TGocciaFFILibraryHandle.Create(LibPath);
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

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.
