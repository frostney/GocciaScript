unit Goccia.ImportMeta;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaImportMetaResolveHelper = class(TGCManagedObject)
  private
    FModuleFilePath: string;
  public
    constructor Create(const AModuleFilePath: string);
    function Resolve(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

function GetOrCreateImportMeta(const AFilePath: string): TGocciaObjectValue;
function FilePathToUrl(const AFilePath: string): string;
procedure ClearImportMetaCache;

implementation

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

const
  PINNED_OBJECTS_PER_MODULE = 3;
  PINNED_GROWTH_MULTIPLIER = 2;
  PINNED_INITIAL_CAPACITY = 8;

var
  ImportMetaCache: TOrderedStringMap<TGocciaObjectValue>;
  PinnedObjects: array of TGCManagedObject;
  PinnedCount: Integer;

// RFC 3986 §2.3 — percent-encode path characters that are not unreserved or '/'
function PercentEncodePath(const APath: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(APath) do
  begin
    Ch := APath[I];
    case Ch of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~', '/',
      ':', '@', '!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '=':
        Result := Result + Ch;
    else
      Result := Result + '%' + IntToHex(Ord(Ch), 2);
    end;
  end;
end;

function FilePathToUrl(const AFilePath: string): string;
var
  AbsolutePath: string;
begin
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

constructor TGocciaImportMetaResolveHelper.Create(const AModuleFilePath: string);
begin
  inherited Create;
  FModuleFilePath := AModuleFilePath;
end;

// ES2026 §13.3.12.1.1 HostGetImportMetaProperties — resolve(specifier)
function TGocciaImportMetaResolveHelper.Resolve(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Specifier, BaseDirectory, ResolvedPath: string;
begin
  if AArgs.Length = 0 then
    ThrowTypeError('import.meta.resolve requires a specifier argument');

  Specifier := AArgs.GetElement(0).ToStringLiteral.Value;

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

// ES2026 §13.3.12.1 Runtime Semantics: Evaluation — ImportMeta
function GetOrCreateImportMeta(const AFilePath: string): TGocciaObjectValue;
var
  MetaObject: TGocciaObjectValue;
  ResolveHelper: TGocciaImportMetaResolveHelper;
  ResolveFunction: TGocciaValue;
  CanonicalPath: string;
begin
  CanonicalPath := ExpandFileName(AFilePath);

  if not Assigned(ImportMetaCache) then
    ImportMetaCache := TOrderedStringMap<TGocciaObjectValue>.Create;

  if ImportMetaCache.TryGetValue(CanonicalPath, Result) then
    Exit;

  // ES2026 §13.3.12.1 step 4a: OrdinaryObjectCreate(null)
  MetaObject := TGocciaObjectValue.Create(nil);

  // ES2026 §13.3.12.1.1 HostGetImportMetaProperties — url
  MetaObject.AssignProperty(PROP_URL,
    TGocciaStringLiteralValue.Create(FilePathToUrl(CanonicalPath)));

  // ES2026 §13.3.12.1.1 HostGetImportMetaProperties — resolve
  ResolveHelper := TGocciaImportMetaResolveHelper.Create(CanonicalPath);
  ResolveFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ResolveHelper.Resolve, PROP_RESOLVE, 1);
  MetaObject.AssignProperty(PROP_RESOLVE, ResolveFunction);

  // Pin the meta object and its dependencies so the GC does not collect them
  if Assigned(TGarbageCollector.Instance) then
  begin
    TGarbageCollector.Instance.PinObject(MetaObject);
    TGarbageCollector.Instance.PinObject(ResolveHelper);
    TGarbageCollector.Instance.PinObject(TGCManagedObject(ResolveFunction));

    if PinnedCount + PINNED_OBJECTS_PER_MODULE > Length(PinnedObjects) then
      SetLength(PinnedObjects, PinnedCount * PINNED_GROWTH_MULTIPLIER + PINNED_INITIAL_CAPACITY);
    PinnedObjects[PinnedCount] := MetaObject;
    PinnedObjects[PinnedCount + 1] := ResolveHelper;
    PinnedObjects[PinnedCount + 2] := TGCManagedObject(ResolveFunction);
    Inc(PinnedCount, PINNED_OBJECTS_PER_MODULE);
  end;

  // ES2026 §13.3.12.1 step 4e: cache on module record
  ImportMetaCache.Add(CanonicalPath, MetaObject);
  Result := MetaObject;
end;

procedure ClearImportMetaCache;
var
  I: Integer;
begin
  if Assigned(ImportMetaCache) then
  begin
    if Assigned(TGarbageCollector.Instance) then
      for I := 0 to PinnedCount - 1 do
        TGarbageCollector.Instance.UnpinObject(PinnedObjects[I]);
    SetLength(PinnedObjects, 0);
    PinnedCount := 0;
    FreeAndNil(ImportMetaCache);
  end;
end;

end.
