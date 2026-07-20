unit Goccia.ImportMeta;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaImportMetaResolveHelper = class(TGCManagedObject)
  private
    FModuleFilePath: string;
    FResolveModuleURL: TResolveModuleURLCallback;
  public
    constructor Create(const AModuleFilePath: string;
      const AResolveModuleURL: TResolveModuleURLCallback);
    function Resolve(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

function GetOrCreateImportMeta(const AFilePath: string;
  const AResolver: TResolveModuleURLCallback = nil): TGocciaObjectValue;
function FilePathToUrl(const AFilePath: string): string;
procedure ClearImportMetaCache;

implementation

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.ThreadCleanupRegistry,
  Goccia.URI,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

const
  PINNED_GROWTH_MULTIPLIER = 2;
  PINNED_INITIAL_CAPACITY = 8;

type
  TGocciaImportMetaCache = class
  private
    FObjects: TOrderedStringMap<TGocciaObjectValue>;
    FPinnedObjects: array of TGCManagedObject;
    FPinnedCount: Integer;
    procedure Pin(const AObject: TGCManagedObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACanonicalPath: string;
      const AMetaObject: TGocciaObjectValue;
      const AResolveHelper: TGocciaImportMetaResolveHelper;
      const AResolveFunction: TGocciaValue);
    function TryGetValue(const ACanonicalPath: string;
      out AMetaObject: TGocciaObjectValue): Boolean;
  end;

threadvar
  FallbackImportMetaCache: TGocciaImportMetaCache;

var
  GImportMetaCacheSlot: TGocciaRealmOwnedSlotId;

constructor TGocciaImportMetaCache.Create;
begin
  inherited Create;
  FObjects := TOrderedStringMap<TGocciaObjectValue>.Create;
end;

destructor TGocciaImportMetaCache.Destroy;
var
  I: Integer;
begin
  if (TGarbageCollector.Instance <> nil) then
    for I := 0 to FPinnedCount - 1 do
      TGarbageCollector.Instance.UnpinObject(FPinnedObjects[I]);
  FPinnedObjects := nil;
  FObjects.Free;
  inherited;
end;

procedure TGocciaImportMetaCache.Pin(const AObject: TGCManagedObject);
begin
  if not Assigned(AObject) or (TGarbageCollector.Instance = nil) then
    Exit;
  TGarbageCollector.Instance.PinObject(AObject);
  if FPinnedCount >= Length(FPinnedObjects) then
    SetLength(FPinnedObjects,
      FPinnedCount * PINNED_GROWTH_MULTIPLIER + PINNED_INITIAL_CAPACITY);
  FPinnedObjects[FPinnedCount] := AObject;
  Inc(FPinnedCount);
end;

procedure TGocciaImportMetaCache.Add(const ACanonicalPath: string;
  const AMetaObject: TGocciaObjectValue;
  const AResolveHelper: TGocciaImportMetaResolveHelper;
  const AResolveFunction: TGocciaValue);
begin
  FObjects.Add(ACanonicalPath, AMetaObject);
  Pin(AMetaObject);
  Pin(AResolveHelper);
  Pin(TGCManagedObject(AResolveFunction));
end;

function TGocciaImportMetaCache.TryGetValue(const ACanonicalPath: string;
  out AMetaObject: TGocciaObjectValue): Boolean;
begin
  Result := FObjects.TryGetValue(ACanonicalPath, AMetaObject);
end;

function GetImportMetaCache: TGocciaImportMetaCache;
var
  Realm: TGocciaRealm;
begin
  Realm := CurrentRealm;
  if Assigned(Realm) then
  begin
    Result := TGocciaImportMetaCache(
      Realm.GetOwnedSlot(GImportMetaCacheSlot));
    if not Assigned(Result) then
    begin
      Result := TGocciaImportMetaCache.Create;
      Realm.SetOwnedSlot(GImportMetaCacheSlot, Result);
    end;
    Exit;
  end;

  if not Assigned(FallbackImportMetaCache) then
    FallbackImportMetaCache := TGocciaImportMetaCache.Create;
  Result := FallbackImportMetaCache;
end;

function HasScheme(const AValue: string): Boolean;
var
  ColonPosition, SlashPosition: NativeInt;
begin
  ColonPosition := Pos(':', AValue);
  SlashPosition := Pos('/', AValue);
  if (ColonPosition = 2) and (Length(AValue) >= 3) and
     (AValue[3] in ['/', '\']) then
    Exit(False);
  Result := (ColonPosition > 1) and
    ((SlashPosition = 0) or (ColonPosition < SlashPosition));
end;

function FilePathToUrl(const AFilePath: string): string;
var
  AbsolutePath: string;
begin
  if HasScheme(AFilePath) then
    Exit(AFilePath);
  AbsolutePath := ExpandFileName(AFilePath);
  {$IFDEF MSWINDOWS}
  AbsolutePath := StringReplace(AbsolutePath, '\', '/', [rfReplaceAll]);
  // RFC 8089 §2 — UNC paths map to file://server/share/..., not file:///
  if (Length(AbsolutePath) >= 2) and (AbsolutePath[1] = '/') and (AbsolutePath[2] = '/') then
    Result := 'file:' + PercentEncodePath(AbsolutePath)
  else
    Result := 'file:///' + PercentEncodePath(AbsolutePath);
  {$ELSE}
  Result := 'file://' + PercentEncodePath(AbsolutePath);
  {$ENDIF}
end;

{ TGocciaImportMetaResolveHelper }

constructor TGocciaImportMetaResolveHelper.Create(
  const AModuleFilePath: string;
  const AResolveModuleURL: TResolveModuleURLCallback);
begin
  inherited Create;
  FModuleFilePath := AModuleFilePath;
  FResolveModuleURL := AResolveModuleURL;
end;

// ES2026 §13.3.12.1.1 HostGetImportMetaProperties(moduleRecord)
function TGocciaImportMetaResolveHelper.Resolve(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Specifier, BaseDirectory, ResolvedPath: string;
begin
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorImportMetaResolveRequiresArg, SSuggestImportMetaUsage);

  Specifier := AArgs.GetElement(0).ToStringLiteral.Value;

  if Assigned(FResolveModuleURL) then
  begin
    Result := TGocciaStringLiteralValue.Create(
      FResolveModuleURL(Specifier, FModuleFilePath));
    Exit;
  end;

  // Absolute specifiers resolve directly; relative ones resolve against the module directory
  if (Length(Specifier) > 0) and ((Specifier[1] = '/') or (Specifier[1] = '\') or
     ((Length(Specifier) >= 2) and (Specifier[2] = ':'))) then
    ResolvedPath := ExpandFileName(Specifier)
  else
  begin
    BaseDirectory := ExtractFilePath(ExpandFileName(FModuleFilePath));
    ResolvedPath := ExpandFileName(IncludeTrailingPathDelimiter(BaseDirectory) + Specifier);
  end;
  Result := TGocciaStringLiteralValue.Create(FilePathToUrl(ResolvedPath));
end;

{ Cache functions }

// ES2026 §13.3.12.1 ImportMeta : import . meta
function GetOrCreateImportMeta(const AFilePath: string;
  const AResolver: TResolveModuleURLCallback): TGocciaObjectValue;
var
  Cache: TGocciaImportMetaCache;
  MetaObject: TGocciaObjectValue;
  ResolveHelper: TGocciaImportMetaResolveHelper;
  ResolveFunction: TGocciaValue;
  CanonicalPath, ResolveBasePath: string;
  Resolver: TResolveModuleURLCallback;
begin
  Resolver := AResolver;
  if Assigned(Resolver) then
    CanonicalPath := Resolver(AFilePath, AFilePath)
  else if HasScheme(AFilePath) then
    CanonicalPath := AFilePath
  else
    CanonicalPath := ExpandFileName(AFilePath);

  Cache := GetImportMetaCache;
  if Cache.TryGetValue(CanonicalPath, Result) then
    Exit;

  // ES2026 §13.3.12.1 step 4a: OrdinaryObjectCreate(null)
  MetaObject := TGocciaObjectValue.Create(nil);

  // ES2026 §13.3.12.1.1 HostGetImportMetaProperties step: url
  if Assigned(Resolver) then
    MetaObject.AssignProperty(PROP_URL,
      TGocciaStringLiteralValue.Create(CanonicalPath))
  else
    MetaObject.AssignProperty(PROP_URL,
      TGocciaStringLiteralValue.Create(FilePathToUrl(CanonicalPath)));

  // ES2026 §13.3.12.1.1 HostGetImportMetaProperties step: resolve
  ResolveBasePath := AFilePath;
  if Assigned(Resolver) and
     (Copy(CanonicalPath, 1, Length('file:')) <> 'file:') then
    ResolveBasePath := CanonicalPath;
  ResolveHelper := TGocciaImportMetaResolveHelper.Create(ResolveBasePath,
    Resolver);
  ResolveFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ResolveHelper.Resolve, PROP_RESOLVE, 1);
  MetaObject.AssignProperty(PROP_RESOLVE, ResolveFunction);

  // ES2026 §13.3.12.1 step 4e: cache on module record
  Cache.Add(CanonicalPath, MetaObject, ResolveHelper, ResolveFunction);
  Result := MetaObject;
end;

procedure ClearImportMetaCache;
begin
  FreeAndNil(FallbackImportMetaCache);
end;

initialization
  GImportMetaCacheSlot := RegisterRealmOwnedSlot('ImportMeta.cache');
  // FPC does not auto-finalize managed threadvars at thread exit. Register the
  // fallback cache clear so the registry drain releases this thread's copy.
  // Normal engine execution stores import.meta state on the active realm and
  // tears it down with that realm; the fallback exists for host calls without
  // an active execution context.
  RegisterThreadvarCleanup(@ClearImportMetaCache);

end.
