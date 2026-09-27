unit Goccia.Builtins.GlobalFFI;

{$I Goccia.inc}

interface

uses
  SysUtils,

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
  { Whether a library path lies inside a materialized provider package. }
  TGocciaFFIProviderPathQuery = function(const APath: string): Boolean
    of object;
  { Raises when ABytes are not the bytes the provider lockfile pins for
    APath. }
  TGocciaFFIProviderVerifier = procedure(const APath: string;
    const ABytes: TBytes) of object;

  TGocciaGlobalFFI = class(TGocciaBuiltin)
  private
    FCapabilities: TGocciaCapabilities;
    FCapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
    FIsProviderPath: TGocciaFFIProviderPathQuery;
    FVerifyProviderBytes: TGocciaFFIProviderVerifier;
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

    { A library inside a provider package is hashed against its pin before
      it is loaded (ADR 0122). Unset, no library is treated as one. }
    property IsProviderPath: TGocciaFFIProviderPathQuery
      read FIsProviderPath write FIsProviderPath;
    property VerifyProviderBytes: TGocciaFFIProviderVerifier
      read FVerifyProviderBytes write FVerifyProviderBytes;
  end;

implementation

uses
  {$IFDEF FPC}{$IFDEF LINUX}
  BaseUnix,
  {$ENDIF}{$ENDIF}
  Classes,

  FileUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.FFI.DynamicLibrary,
  Goccia.FFI.Types,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFILibrary,
  Goccia.Values.FFIPointer,
  Goccia.Values.FFIType,
  Goccia.URI,
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
  LibPath, LoadPath, DenialDetail, PinnedLoadPath, PinnedPath,
    ReadError, RequestedPath, URLPath: string;
  Allowed, IsBareName, IsProviderLibrary: Boolean;
  LibraryBytes: TBytes;
  Handle: TGocciaFFILibraryHandle;
  {$IFDEF FPC}{$IFDEF LINUX}
  PinnedDescriptor: LongInt;
  {$ENDIF}{$ENDIF}
  {$IFDEF MSWINDOWS}
  PinnedHandle: THandle;
  HoldingPin: Boolean;
  {$ENDIF}

  { Verify on load: a library inside a provider package must be the bytes
    its lockfile pins. The bytes are read through the pinned descriptor or
    handle where the platform has one. That fixes which file is loaded, not
    its contents: the file can still be rewritten in place between the hash
    and the load, the same window macOS has. }
  procedure VerifyProviderLibrary(const ABytesRead: Boolean;
    const ABytes: TBytes);
  begin
    if not ABytesRead then
      ThrowTypeError('Failed to load library: ' + LibPath,
        SSuggestFFILibraryOpen);
    try
      FVerifyProviderBytes(LoadPath, ABytes);
    except
      on E: Exception do
      begin
        if Assigned(FCapabilityAuditEmitter) then
          FCapabilityAuditEmitter(gckFFIOpen, gcdDeny, LibPath, E.Message);
        ThrowTypeError(E.Message, SSuggestFFILibraryOpen);
      end;
    end;
  end;

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
    Reported := CanonicalCapabilityPath(RequestedPath);
    Result := Reported <> LoadPath;
      {$ENDIF}
    {$ENDIF}
  end;

begin
  if AArgs.Length < 1 then
    ThrowTypeError(SErrorFFIOpenRequiresPath, SSuggestFFILibraryOpen);

  LibPath := AArgs.GetElement(0).ToStringLiteral.Value;
  { A `file:` URL (a URL object or its string) names the host path it
    encodes, so package code can open a library beside itself with
    `new URL("./lib.so", import.meta.url)`. It is judged like that path. }
  LoadPath := LibPath;
  if IsFileURL(LibPath) then
  begin
    if not TryFileURLToHostPath(LibPath, URLPath) then
      ThrowTypeError(Format(SErrorFFIOpenFileURL, [LibPath]),
        SSuggestFFILibraryOpen);
    LoadPath := URLPath;
  end;
  RequestedPath := LoadPath;
  IsBareName := IsBareLibraryName(LoadPath);
  { A name with no directory part is found by the platform loader's search
    path, which no path scope describes, so only an unscoped grant with no
    deny scope covers it. Anything else is judged, and then loaded, at its
    canonical path, so the file checked is the file opened. }
  if IsBareName then
  begin
    Allowed := FCapabilities.AllowsUnscoped(gcFFI);
    { With a deny scope in force, asking for the unscoped grant would not
      help. (An unscoped deny never gets here: FFI is not installed.) }
    if FCapabilities.HasDeny(gcFFI) then
      DenialDetail := SSuggestFFIBareNameDenyScope
    else
      DenialDetail := SSuggestFFIBareName;
  end
  else
  begin
    LoadPath := CanonicalCapabilityPath(LoadPath);
    Allowed := FCapabilities.AllowsPath(gcFFI, LoadPath);
    if FCapabilities.DeniesPath(gcFFI, LoadPath) then
      DenialDetail := Format(SSuggestFFIDenied, [LoadPath])
    else
      DenialDetail := Format(SSuggestFFINotGranted, [LoadPath,
        ExtractFileDir(LoadPath)]);
  end;
  if not Allowed then
    Deny(DenialDetail, NOT_COVERED_REASON);
  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFFIOpen, gcdAllow, LibPath,
      'the ffi capability covers this library');

  IsProviderLibrary := (not IsBareName) and Assigned(FIsProviderPath) and
    Assigned(FVerifyProviderBytes) and FIsProviderPath(LoadPath);
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
    if IsProviderLibrary then
      try
        VerifyProviderLibrary(ReadHostHandleBytes(PinnedDescriptor,
          LibraryBytes, ReadError), LibraryBytes);
      except
        FpClose(PinnedDescriptor);
        raise;
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
    if IsProviderLibrary then
      try
        VerifyProviderLibrary(ReadHostHandleBytes(PinnedHandle, LibraryBytes,
          ReadError), LibraryBytes);
      except
        ClosePinnedHostFile(PinnedHandle);
        raise;
      end;
    { Load the pinned file by its own final path: fully resolved (no
      junction or symlink left in it) and exactly the string just judged. }
    PinnedLoadPath := PinnedPath;
  end;
  {$ENDIF}
  {$IF NOT DEFINED(LINUX) AND NOT DEFINED(MSWINDOWS)}
  { No descriptor the loader can take: the path's bytes are hashed just
    before the load, which leaves the window the post-load path check
    describes. }
  if IsProviderLibrary then
  begin
    try
      LibraryBytes := ReadFileBytes(LoadPath);
    except
      on E: EStreamError do
        VerifyProviderLibrary(False, nil);
    end;
    VerifyProviderLibrary(True, LibraryBytes);
  end;
  {$IFEND}
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
