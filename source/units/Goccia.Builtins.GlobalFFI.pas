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

var
  { Test seam: called after FFI.open's capability check passes and before
    the library is loaded, so a test can change the filesystem in between.
    Nil outside tests. }
  GocciaFFIAfterOpenCheck: procedure = nil;

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
  {$IFDEF FPC}{$IFDEF LINUX}
  BaseUnix,
  {$ENDIF}{$ENDIF}
  SysUtils,

  {$IFDEF MSWINDOWS}
  FileUtils,
  {$ENDIF}

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

{ FFI.open judges the library's canonical path and then loads it. Between
  the two a directory on that path could be swapped, so the load is pinned
  to what was judged where the platform allows:

  - Linux: the file is opened first, the kernel's path for that descriptor
    is judged verbatim, and the loader maps the descriptor itself
    (/proc/self/fd/N), so what is loaded is exactly what was judged.
  - Windows: the file is opened first without write or delete sharing, so
    neither it nor any directory on its path can be renamed or replaced
    while the handle is held; the opened file's final path is judged
    verbatim, the library is loaded by that same final path while the
    handle is held, and the path the loader reports for the module is
    judged once more.
  - Elsewhere (macOS, BSD): no pre-load pin is available. After loading,
    the path is canonicalized again and must still be the judged one, and
    a mismatch unloads and refuses the library — but a swapped library's
    initializers (Mach-O/ELF constructors) have already run by then, and a
    swap undone again inside the load window is not detected at all. }
function TGocciaGlobalFFI.FFIOpen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
const
  NOT_COVERED_REASON = 'the ffi capability does not cover this library';
  CHANGED_REASON = 'the library changed between the ffi check and the load';
var
  LibPath, LoadPath, DenialDetail, PinnedLoadPath, PinnedPath: string;
  Allowed, IsBareName: Boolean;
  Handle: TGocciaFFILibraryHandle;
  {$IFDEF FPC}{$IFDEF LINUX}
  PinnedDescriptor: LongInt;
  {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS}
  PinnedHandle: THandle;
  HoldingPin: Boolean;
  {$ENDIF}

  procedure Deny(const ADetail, AReason: string);
  begin
    if Assigned(FCapabilityAuditEmitter) then
      FCapabilityAuditEmitter(gckFFIOpen, gcdDeny, LibPath, AReason);
    ThrowPermissionDenied(CapabilityName(gcFFI), LibPath, ADetail);
  end;

  function LoadedOutsideJudgedPath: Boolean;
  var
    Reported: string;
  begin
    {$IFDEF MSWINDOWS}
    Reported := Handle.LoadedPath;
    Result := (Reported = '') or
      not FCapabilities.AllowsPath(gcFFI, Reported);
    {$ELSE}
      {$IFDEF LINUX}
    Reported := '';
    Result := False;
      {$ELSE}
    Reported := CanonicalCapabilityPath(LibPath);
    Result := Reported <> LoadPath;
      {$ENDIF}
    {$ENDIF}
  end;

begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFIOpenRequiresPath, SSuggestFFILibraryOpen);

  LibPath := AArgs.GetElement(0).ToStringLiteral.Value;
  IsBareName := IsBareLibraryName(LibPath);
  { A name with no directory part is found by the platform loader's search
    path, which no path scope describes, so only an unscoped grant with no
    deny scope covers it. Anything else is judged, and then loaded, at its
    canonical path, so the file checked is the file opened. }
  if IsBareName then
  begin
    LoadPath := LibPath;
    Allowed := FCapabilities.AllowsUnscoped(gcFFI);
    DenialDetail := 'a library name searched for by the platform loader ' +
      'needs an unscoped ffi grant';
  end
  else
  begin
    LoadPath := CanonicalCapabilityPath(LibPath);
    Allowed := FCapabilities.AllowsPath(gcFFI, LoadPath);
    DenialDetail := Format('the ffi capability does not cover %s',
      [LoadPath]);
  end;
  if not Allowed then
    Deny(DenialDetail, NOT_COVERED_REASON);
  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFFIOpen, gcdAllow, LibPath,
      'the ffi capability covers this library');

  PinnedLoadPath := LoadPath;
  PinnedPath := '';
  {$IFDEF FPC}{$IFDEF LINUX}
  PinnedDescriptor := -1;
  if not IsBareName then
  begin
    PinnedDescriptor := FpOpen(LoadPath, O_RDONLY);
    if PinnedDescriptor < 0 then
      ThrowTypeError('Failed to load library: ' + LibPath,
        SSuggestFFILibraryOpen);
    PinnedLoadPath := '/proc/self/fd/' + IntToStr(PinnedDescriptor);
    PinnedPath := fpReadLink(PinnedLoadPath);
    if (PinnedPath = '') or
       not FCapabilities.AllowsCanonicalPath(gcFFI, PinnedPath) then
    begin
      FpClose(PinnedDescriptor);
      Deny(Format('the ffi capability does not cover %s', [PinnedPath]),
        CHANGED_REASON);
    end;
  end;
  {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS}
  HoldingPin := False;
  PinnedHandle := 0;
  if not IsBareName then
  begin
    if not OpenPinnedHostFile(LoadPath, PinnedHandle, PinnedPath) then
      ThrowTypeError('Failed to load library: ' + LibPath,
        SSuggestFFILibraryOpen);
    HoldingPin := True;
    if not FCapabilities.AllowsCanonicalPath(gcFFI, PinnedPath) then
    begin
      ClosePinnedHostFile(PinnedHandle);
      Deny(Format('the ffi capability does not cover %s', [PinnedPath]),
        CHANGED_REASON);
    end;
    { Load the pinned file by its own final path: fully resolved (no
      junction or symlink left in it) and exactly the string just judged. }
    PinnedLoadPath := PinnedPath;
  end;
  {$ENDIF}
  try
    if Assigned(GocciaFFIAfterOpenCheck) then
      GocciaFFIAfterOpenCheck;

    try
      Handle := TGocciaFFILibraryHandle.Create(LibPath, PinnedLoadPath);
    except
      on E: Exception do
        ThrowTypeError(E.Message, SSuggestFFILibraryOpen);
    end;
  finally
    {$IFDEF FPC}{$IFDEF LINUX}
    if PinnedDescriptor >= 0 then
      FpClose(PinnedDescriptor);
    {$ENDIF}{$ENDIF}
    {$IFDEF MSWINDOWS}
    if HoldingPin then
      ClosePinnedHostFile(PinnedHandle);
    {$ENDIF}
  end;

  if (not IsBareName) and LoadedOutsideJudgedPath then
  begin
    Handle.ReleaseOwner;
    Deny(CHANGED_REASON, CHANGED_REASON);
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
