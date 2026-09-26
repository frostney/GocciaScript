unit Goccia.CLI.Permissions;

{ A config file's `permissions` block, read into a request (ADR 0122).

  A block lists `allow-<cap>` and `deny-<cap>` keys for read, net, ffi, and
  import. Each value is `true` (every scope), `false` (absent; lets a child
  config cancel a base), or an array of scope strings. Relative path scopes,
  and the directory of `node_modules=<dir>`, resolve against the directory of
  the file that declares them, so a scope means the same place wherever the
  command runs from. With `extends`, the child overrides the base key by key.

  The top-level `unsafe-function-constructor` and `unsafe-shadowrealm` keys
  are requests too: they enable dynamic code generation, so they need the same
  acceptance as an allow. So is a `sandbox` section, which GocciaRunner reads
  to copy host files into its sandbox and write changed ones back.

  A request is not a grant: the application decides whether to accept it. The
  config trust store (Goccia.CLI.Trust) records a SHA-256 of the request's
  normalized block, so any change to the block needs trusting again. }

{$I Goccia.inc}

interface

uses
  CLI.ConfigFile,
  CLI.Options,

  Goccia.Capabilities;

type
  TGocciaHonoredCapabilities = set of TGocciaCapability;

  { The `unsafe-*` config keys, which enable dynamic code generation. }
  TGocciaUnsafeRequest = (gurFunctionConstructor, gurShadowRealm);
  TGocciaUnsafeRequests = set of TGocciaUnsafeRequest;

  TGocciaPermissionScopes = record
    { The key appears in the effective config (even as false or []). }
    Declared: Boolean;
    Unscoped: Boolean;
    Scopes: TGocciaCapabilityScopes;
    { The file whose value is in effect. }
    SourcePath: string;
    function RequestsAny: Boolean;
  end;

  { One `copy` or `copy-rw` entry of a config's `sandbox` section, in the
    `--copy` grammar `<host>[=<sandbox>]`. }
  TGocciaSandboxInputRequest = record
    { The entry as written. }
    Spec: string;
    { The host part, made absolute against the declaring file's directory. }
    HostPath: string;
    { The sandbox part as written; '' when the entry has none. }
    SandboxPath: string;
    ReadWrite: Boolean;
    { The file that declares the entry. }
    SourcePath: string;
  end;
  TGocciaSandboxInputRequests = array of TGocciaSandboxInputRequest;

  { A config's `sandbox` section (GocciaRunner's sandbox mode). With
    `extends`, the child overrides the base key by key. }
  TGocciaSandboxRequest = record
    { The effective config has a `sandbox` object, even an empty one. }
    Declared: Boolean;
    { The nearest file that declares a `sandbox` object. }
    SourcePath: string;
    { `copy` then `copy-rw` entries, each in declaration order. }
    Inputs: TGocciaSandboxInputRequests;
    Entry: string;
    { '', SANDBOX_DIFF_DEFAULT (`true`: a diff in the default format, or the
      one the diff file's extension names), `json`, or `unified`. }
    Diff: string;
    { Absolute, against the declaring file's directory; '' when absent. }
    DiffFile: string;
    DiffFileSourcePath: string;
  end;

  TGocciaConfigPermissionRequest = record
    ConfigPath: string;
    Allow: array[TGocciaCapability] of TGocciaPermissionScopes;
    Deny: array[TGocciaCapability] of TGocciaPermissionScopes;
    { The unsafe-* keys set to true in the effective config. }
    Unsafe: TGocciaUnsafeRequests;
    Sandbox: TGocciaSandboxRequest;
    class function Empty: TGocciaConfigPermissionRequest; static;
    { True when some allow-* key asks for something, an unsafe-* key is true,
      or a sandbox section is declared. A deny-only block asks for nothing. }
    function RequestsGrants: Boolean;
    { As RequestsGrants, counting only the capabilities in AHonored, the
      unsafe-* keys only when AHonorsUnsafe, and a sandbox section only when
      AHonorsSandbox. }
    function RequestsHonoredGrants(
      const AHonored: TGocciaHonoredCapabilities;
      const AHonorsUnsafe: Boolean = True;
      const AHonorsSandbox: Boolean = False): Boolean;
  end;

  { A malformed permissions block: an unknown key or a value of the wrong
    shape. A usage error, so the run exits 2. }
  EGocciaConfigPermissionError = class(TCLIUsageError);

const
  PERMISSIONS_CONFIG_KEY = 'permissions';
  SANDBOX_CONFIG_KEY = 'sandbox';
  { TGocciaSandboxRequest.Diff for `"diff": true`. }
  SANDBOX_DIFF_DEFAULT = 'true';
  { What an import scope must name, for the missing-scope errors. }
  IMPORT_SCOPE_REQUIREMENT =
    'node_modules[=<dir>] or a provider such as github';
  ALL_CAPABILITIES: TGocciaHonoredCapabilities = [gcRead, gcNet, gcFFI,
    gcImport];
  { The config keys, which are also the command-line flag names. }
  UNSAFE_REQUEST_KEYS: array[TGocciaUnsafeRequest] of string = (
    'unsafe-function-constructor', 'unsafe-shadowrealm');
  { Part of every normalized block, so a later change to the normalization
    cannot collide with a block trusted under this one. }
  PERMISSION_BLOCK_VERSION = 1;
  { `-P`: accept every config permission request for one run. }
  ACCEPT_CONFIG_PERMISSIONS_FLAG = 'accept-config-permissions';
  ACCEPT_CONFIG_PERMISSIONS_SHORT_FLAG = 'P';

{ `allow-read`, `deny-net`, ... }
function PermissionKeyName(const AAllow: Boolean;
  const ACapability: TGocciaCapability): string;

{ Makes a scope as written absolute where the capability takes a path: read
  and ffi paths, and the directory of `node_modules=<dir>`. ABaseDirectory is
  the working directory for a command-line scope and the declaring file's
  directory for a config scope. net scopes and import providers are returned
  trimmed. }
function ResolvePermissionScope(const ACapability: TGocciaCapability;
  const AScope, ABaseDirectory: string): string;

{ The request declared by AEntries, the entries of the config at
  AConfigPath (ParseConfigFile order: a child's entries before its base's).
  Raises EGocciaConfigPermissionError for an unknown key or a value of the
  wrong shape, and TParseError for a scope the capability does not accept. }
function ReadConfigPermissionRequest(const AEntries: TConfigEntryArray;
  const AConfigPath: string): TGocciaConfigPermissionRequest;

{ One message per capability the request asks to allow that AHonored does
  not include, per unsafe-* request when not AHonorsUnsafe, and for a sandbox
  section when not AHonorsSandbox:
  `requests allow-<cap>, which <Program> cannot grant; ignoring it`. Callers
  prefix the config path in their own output format. }
function UnsupportedRequestWarnings(
  const ARequest: TGocciaConfigPermissionRequest;
  const AHonored: TGocciaHonoredCapabilities;
  const AProgramName: string;
  const AHonorsUnsafe: Boolean = True;
  const AHonorsSandbox: Boolean = False): TGocciaCapabilityScopes;

{ The request as canonical JSON: compact, object keys in byte order, each
  capability's scopes deduplicated and sorted, a capability with an unscoped
  entry collapsed to `true`, undeclared, false, and empty keys omitted, and a
  `"version"`. Path scopes are the absolute, lexically normalized paths the
  request already holds, so the block does not change when a path it names
  appears, disappears, or becomes a symbolic link. docs/permissions.md shows
  an example. }
function NormalizedPermissionBlock(
  const ARequest: TGocciaConfigPermissionRequest): string;
{ SHA256Hex of the UTF-8 NormalizedPermissionBlock. }
function PermissionBlockHash(
  const ARequest: TGocciaConfigPermissionRequest): string;
{ One line per key of a normalized block, such as
  `allow-net: 127.0.0.1, example.com` or `allow-ffi: any library`. Empty for
  text that is not JSON. }
function PermissionBlockLines(const ABlockJSON: string): TGocciaCapabilityScopes;
{ The lines of ARequest's normalized block, each prefixed with AIndent and
  ended with a line break. }
function DescribePermissionRequest(
  const ARequest: TGocciaConfigPermissionRequest;
  const AIndent: string): string;
{ The current request's lines against a previously trusted block: an
  unchanged line is prefixed with AIndent, and an added or removed line with
  `+ ` or `- ` in place of AIndent's last two characters. Removed lines come
  first. }
function DescribePermissionChange(const APreviousBlockJSON: string;
  const ACurrent: TGocciaConfigPermissionRequest;
  const AIndent: string): string;

{ The parenthesized advice of an invalid-scope error for ACapability. }
function PermissionScopeHint(const ACapability: TGocciaCapability): string;

{ `read, net, and ffi`; `no capability flags` for an empty set. }
function DescribeCapabilities(const ACapabilities: TGocciaHonoredCapabilities):
  string;

implementation

uses
  Classes,
  SysUtils,

  FileUtils,
  JSONParser,
  SHA256,
  TextEncoding,

  Goccia.JSON.Utils;

const
  PERMISSION_KEY_PREFIX = PERMISSIONS_CONFIG_KEY + '.';
  ALLOW_PREFIX = 'allow-';
  DENY_PREFIX = 'deny-';
  NODE_MODULES_CEILING_PREFIX = IMPORT_NODE_MODULES_SCOPE + '=';
  NET_SCOPE_HINT =
    'use host, host:port, *.domain, an IP, a CIDR range, or private';
  IMPORT_SCOPE_HINT =
    'use node_modules, node_modules=<dir>, or a provider such as github';

function PermissionKeyName(const AAllow: Boolean;
  const ACapability: TGocciaCapability): string;
begin
  if AAllow then
    Result := ALLOW_PREFIX + CapabilityName(ACapability)
  else
    Result := DENY_PREFIX + CapabilityName(ACapability);
end;

function ValidPermissionKeys: string;
var
  Allow: Boolean;
  Capability: TGocciaCapability;
begin
  Result := '';
  for Allow := True downto False do
    for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + PermissionKeyName(Allow, Capability);
    end;
end;

function TryParsePermissionKey(const AKey: string; out AAllow: Boolean;
  out ACapability: TGocciaCapability): Boolean;
begin
  ACapability := gcRead;
  AAllow := False;
  if Copy(AKey, 1, Length(ALLOW_PREFIX)) = ALLOW_PREFIX then
  begin
    AAllow := True;
    Result := TryParseCapabilityName(Copy(AKey, Length(ALLOW_PREFIX) + 1,
      MaxInt), ACapability);
  end
  else if Copy(AKey, 1, Length(DENY_PREFIX)) = DENY_PREFIX then
    Result := TryParseCapabilityName(Copy(AKey, Length(DENY_PREFIX) + 1,
      MaxInt), ACapability)
  else
    Result := False;
  { Capability names are matched exactly in config keys. }
  if Result and (AKey <> PermissionKeyName(AAllow, ACapability)) then
    Result := False;
end;

function AbsolutePath(const APath, ABaseDirectory: string): string;
begin
  if IsAbsoluteHostPath(APath) then
    Result := ExpandHostFileName(APath)
  else
    Result := ExpandHostFileName(IncludeTrailingPathDelimiter(ABaseDirectory) +
      APath);
  Result := ExcludeTrailingPathDelimiter(Result);
  if Result = '' then
    Result := PathDelim;
end;

function ResolvePermissionScope(const ACapability: TGocciaCapability;
  const AScope, ABaseDirectory: string): string;
var
  Scope: string;
begin
  Scope := Trim(AScope);
  case ACapability of
    gcRead, gcFFI:
      Result := AbsolutePath(Scope, ABaseDirectory);
    gcImport:
      if SameText(Copy(Scope, 1, Length(NODE_MODULES_CEILING_PREFIX)),
         NODE_MODULES_CEILING_PREFIX) and
         (Length(Scope) > Length(NODE_MODULES_CEILING_PREFIX)) then
        Result := NODE_MODULES_CEILING_PREFIX + AbsolutePath(Copy(Scope,
          Length(NODE_MODULES_CEILING_PREFIX) + 1, MaxInt), ABaseDirectory)
      else
        Result := Scope;
  else
    Result := Scope;
  end;
end;

{ TGocciaPermissionScopes }

function TGocciaPermissionScopes.RequestsAny: Boolean;
begin
  Result := Unscoped or (Length(Scopes) > 0);
end;

{ TGocciaConfigPermissionRequest }

class function TGocciaConfigPermissionRequest.Empty:
  TGocciaConfigPermissionRequest;
begin
  Result := Default(TGocciaConfigPermissionRequest);
end;

function TGocciaConfigPermissionRequest.RequestsGrants: Boolean;
begin
  Result := RequestsHonoredGrants(ALL_CAPABILITIES, True, True);
end;

function TGocciaConfigPermissionRequest.RequestsHonoredGrants(
  const AHonored: TGocciaHonoredCapabilities;
  const AHonorsUnsafe, AHonorsSandbox: Boolean): Boolean;
var
  Capability: TGocciaCapability;
begin
  if AHonorsUnsafe and (Unsafe <> []) then
    Exit(True);
  if AHonorsSandbox and Sandbox.Declared then
    Exit(True);
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if (Capability in AHonored) and Allow[Capability].RequestsAny then
      Exit(True);
  Result := False;
end;

function PermissionScopeHint(const ACapability: TGocciaCapability): string;
begin
  case ACapability of
    gcNet:
      Result := NET_SCOPE_HINT;
    gcImport:
      Result := IMPORT_SCOPE_HINT;
  else
    Result := 'use a path';
  end;
end;

procedure ValidateScope(const ALocation, AKey: string;
  const ACapability: TGocciaCapability; const AScope, AWritten: string);
begin
  try
    TGocciaCapabilities.None.Allow(ACapability, AScope);
  except
    on E: EGocciaCapabilityScopeError do
      raise TParseError.CreateFmt('%s: invalid scope in "%s": "%s" (%s)',
        [ALocation, AKey, AWritten, PermissionScopeHint(ACapability)]);
  end;
end;

function TryParseUnsafeKey(const AKey: string;
  out ARequest: TGocciaUnsafeRequest): Boolean;
var
  Request: TGocciaUnsafeRequest;
begin
  ARequest := gurFunctionConstructor;
  for Request := Low(TGocciaUnsafeRequest) to High(TGocciaUnsafeRequest) do
    if AKey = UNSAFE_REQUEST_KEYS[Request] then
    begin
      ARequest := Request;
      Exit(True);
    end;
  Result := False;
end;

const
  SANDBOX_KEY_PREFIX = SANDBOX_CONFIG_KEY + '.';
  SANDBOX_COPY_KEY = 'copy';
  SANDBOX_COPY_READ_WRITE_KEY = 'copy-rw';
  SANDBOX_ENTRY_KEY = 'entry';
  SANDBOX_DIFF_KEY = 'diff';
  SANDBOX_DIFF_FILE_KEY = 'diff-file';
  SANDBOX_REMOVED_FILES_KEY = 'files';
  SANDBOX_VALID_KEYS = 'copy, copy-rw, entry, diff, diff-file';
  DIFF_FORMAT_JSON = 'json';
  DIFF_FORMAT_UNIFIED = 'unified';

type
  TSandboxKey = (skCopy, skCopyReadWrite, skEntry, skDiff, skDiffFile);
  TSandboxKeys = set of TSandboxKey;

function TryParseSandboxKey(const AKey: string; out ASandboxKey: TSandboxKey):
  Boolean;
begin
  Result := True;
  ASandboxKey := skCopy;
  if AKey = SANDBOX_COPY_KEY then
    ASandboxKey := skCopy
  else if AKey = SANDBOX_COPY_READ_WRITE_KEY then
    ASandboxKey := skCopyReadWrite
  else if AKey = SANDBOX_ENTRY_KEY then
    ASandboxKey := skEntry
  else if AKey = SANDBOX_DIFF_KEY then
    ASandboxKey := skDiff
  else if AKey = SANDBOX_DIFF_FILE_KEY then
    ASandboxKey := skDiffFile
  else
    Result := False;
end;

{ Splits `<host>[=<sandbox>]` at its first `=`. }
procedure SplitSandboxInputSpec(const ASpec: string; out AHost,
  ASandbox: string);
var
  Separator: Integer;
begin
  Separator := Pos('=', ASpec);
  if Separator > 0 then
  begin
    AHost := Copy(ASpec, 1, Separator - 1);
    ASandbox := Copy(ASpec, Separator + 1, MaxInt);
  end
  else
  begin
    AHost := ASpec;
    ASandbox := '';
  end;
end;

{ Reads the `sandbox` and `sandbox.*` entries into ARequest. Entries arrive
  child first, so the first file to declare a key keeps it. }
procedure ReadSandboxEntry(var ARequest: TGocciaSandboxRequest;
  var ADeclaredKeys: TSandboxKeys; var AKeySources: array of string;
  const AEntry: TConfigEntry; const ALocation: string);
var
  SubKey, HostPart, SandboxPart: string;
  SandboxKey: TSandboxKey;
  Overridden: Boolean;
  Input: TGocciaSandboxInputRequest;

  procedure RequireString(const AAllowArray: Boolean);
  begin
    if (AEntry.Kind <> cvkString) or (AEntry.InArray and not AAllowArray) then
    begin
      if AAllowArray then
        raise EGocciaConfigPermissionError.CreateFmt(
          '%s: "%s" must be a string or an array of strings in the --copy ' +
          'grammar <host>[=<sandbox>]', [ALocation, AEntry.Key])
      else
        raise EGocciaConfigPermissionError.CreateFmt(
          '%s: "%s" must be a string', [ALocation, AEntry.Key]);
    end;
  end;

begin
  if AEntry.Key = SANDBOX_CONFIG_KEY then
  begin
    if AEntry.Kind <> cvkObject then
      raise EGocciaConfigPermissionError.CreateFmt(
        '%s: "%s" must be an object with %s keys',
        [ALocation, SANDBOX_CONFIG_KEY, SANDBOX_VALID_KEYS]);
    if not ARequest.Declared then
    begin
      ARequest.Declared := True;
      ARequest.SourcePath := ALocation;
    end;
    Exit;
  end;

  if not ARequest.Declared then
  begin
    ARequest.Declared := True;
    ARequest.SourcePath := ALocation;
  end;
  SubKey := Copy(AEntry.Key, Length(SANDBOX_KEY_PREFIX) + 1, MaxInt);
  if SubKey = SANDBOX_REMOVED_FILES_KEY then
    raise EGocciaConfigPermissionError.CreateFmt(
      '%s: "%s.%s" was removed in GocciaScript %s; list host inputs as ' +
      '"copy" or "copy-rw" strings (<host>[=<sandbox>]) instead',
      [ALocation, SANDBOX_CONFIG_KEY, SANDBOX_REMOVED_FILES_KEY,
       OPTIONS_REMOVED_IN_VERSION]);
  if not TryParseSandboxKey(SubKey, SandboxKey) then
    raise EGocciaConfigPermissionError.CreateFmt(
      '%s: unknown sandbox key "%s" (valid: %s)',
      [ALocation, SubKey, SANDBOX_VALID_KEYS]);

  Overridden := (SandboxKey in ADeclaredKeys) and
    (AKeySources[Ord(SandboxKey)] <> AEntry.SourcePath);
  if not (SandboxKey in ADeclaredKeys) then
  begin
    Include(ADeclaredKeys, SandboxKey);
    AKeySources[Ord(SandboxKey)] := AEntry.SourcePath;
  end;

  case SandboxKey of
    skCopy, skCopyReadWrite:
    begin
      if AEntry.Kind = cvkEmptyArray then
        Exit;
      RequireString(True);
      if Trim(AEntry.Value) = '' then
        raise EGocciaConfigPermissionError.CreateFmt(
          '%s: "%s" has an empty entry', [ALocation, AEntry.Key]);
      if Overridden then
        Exit;
      SplitSandboxInputSpec(AEntry.Value, HostPart, SandboxPart);
      if HostPart = '' then
        raise EGocciaConfigPermissionError.CreateFmt(
          '%s: "%s" entry "%s" names no host path',
          [ALocation, AEntry.Key, AEntry.Value]);
      Input.Spec := AEntry.Value;
      Input.HostPath := AbsolutePath(HostPart, ExtractFilePath(ALocation));
      Input.SandboxPath := SandboxPart;
      Input.ReadWrite := SandboxKey = skCopyReadWrite;
      Input.SourcePath := ALocation;
      SetLength(ARequest.Inputs, Length(ARequest.Inputs) + 1);
      ARequest.Inputs[High(ARequest.Inputs)] := Input;
    end;
    skEntry:
    begin
      RequireString(False);
      if not Overridden then
        ARequest.Entry := AEntry.Value;
    end;
    skDiff:
    begin
      if (AEntry.Kind = cvkBoolean) and not AEntry.InArray then
      begin
        if not Overridden and (AEntry.Value = SANDBOX_DIFF_DEFAULT) then
          ARequest.Diff := SANDBOX_DIFF_DEFAULT;
      end
      else if (AEntry.Kind = cvkString) and not AEntry.InArray and
        ((AEntry.Value = DIFF_FORMAT_JSON) or
         (AEntry.Value = DIFF_FORMAT_UNIFIED)) then
      begin
        if not Overridden then
          ARequest.Diff := AEntry.Value;
      end
      else
        raise EGocciaConfigPermissionError.CreateFmt(
          '%s: "%s" must be true, false, "json", or "unified"',
          [ALocation, AEntry.Key]);
    end;
    skDiffFile:
    begin
      RequireString(False);
      if Trim(AEntry.Value) = '' then
        raise EGocciaConfigPermissionError.CreateFmt(
          '%s: "%s" names no file', [ALocation, AEntry.Key]);
      if not Overridden then
      begin
        ARequest.DiffFile := AbsolutePath(AEntry.Value,
          ExtractFilePath(ALocation));
        ARequest.DiffFileSourcePath := ALocation;
      end;
    end;
  end;
end;

function ReadConfigPermissionRequest(const AEntries: TConfigEntryArray;
  const AConfigPath: string): TGocciaConfigPermissionRequest;
var
  I: Integer;
  Allow: Boolean;
  Capability: TGocciaCapability;
  SubKey, Location, Scope: string;
  Entry: TConfigEntry;
  Scopes: TGocciaPermissionScopes;
  UnsafeRequest: TGocciaUnsafeRequest;
  UnsafeSeen: TGocciaUnsafeRequests;
  Overridden: Boolean;
  SandboxKeys: TSandboxKeys;
  SandboxKeySources: array[TSandboxKey] of string;
begin
  Result := TGocciaConfigPermissionRequest.Empty;
  Result.ConfigPath := AConfigPath;
  UnsafeSeen := [];
  SandboxKeys := [];
  for I := 0 to High(AEntries) do
  begin
    Entry := AEntries[I];
    Location := Entry.SourcePath;
    if Location = '' then
      Location := AConfigPath;

    if TryParseUnsafeKey(Entry.Key, UnsafeRequest) then
    begin
      { Entries arrive child first, so the nearest file's value wins. }
      if UnsafeRequest in UnsafeSeen then
        Continue;
      Include(UnsafeSeen, UnsafeRequest);
      if (Entry.Value <> 'true') and (Entry.Value <> 'false') then
        raise TParseError.CreateFmt('%s: "%s" must be true or false, got "%s"',
          [Location, Entry.Key, Entry.Value]);
      if Entry.Value = 'true' then
        Include(Result.Unsafe, UnsafeRequest);
      Continue;
    end;

    if (Entry.Key = SANDBOX_CONFIG_KEY) or
       (Copy(Entry.Key, 1, Length(SANDBOX_KEY_PREFIX)) = SANDBOX_KEY_PREFIX) then
    begin
      ReadSandboxEntry(Result.Sandbox, SandboxKeys, SandboxKeySources, Entry,
        Location);
      Continue;
    end;

    if (Entry.Key = PERMISSIONS_CONFIG_KEY) and (Entry.Kind <> cvkObject) then
      raise EGocciaConfigPermissionError.CreateFmt(
        '%s: "%s" must be an object of allow-* and deny-* keys',
        [Location, PERMISSIONS_CONFIG_KEY]);
    if Copy(Entry.Key, 1, Length(PERMISSION_KEY_PREFIX)) <>
       PERMISSION_KEY_PREFIX then
      Continue;

    SubKey := Copy(Entry.Key, Length(PERMISSION_KEY_PREFIX) + 1, MaxInt);
    if not TryParsePermissionKey(SubKey, Allow, Capability) then
      raise EGocciaConfigPermissionError.CreateFmt(
        '%s: unknown permission "%s" (valid: %s)',
        [Location, SubKey, ValidPermissionKeys]);

    if Allow then
      Scopes := Result.Allow[Capability]
    else
      Scopes := Result.Deny[Capability];
    { Entries arrive child first: a key a nearer file declared is not
      extended by its base's value for the same key. The value is still
      validated, so a malformed base fails wherever it is used. }
    Overridden := Scopes.Declared and (Scopes.SourcePath <> Entry.SourcePath);
    if not Overridden then
    begin
      Scopes.Declared := True;
      Scopes.SourcePath := Entry.SourcePath;
    end;

    if (Entry.Kind = cvkBoolean) and not Entry.InArray then
    begin
      if Entry.Value = 'true' then
      begin
        if Capability = gcImport then
          raise EGocciaConfigPermissionError.CreateFmt(
            '%s: "%s" needs scopes: %s',
            [Location, Entry.Key, IMPORT_SCOPE_REQUIREMENT]);
        Scopes.Unscoped := True;
      end;
    end
    else if Entry.Kind = cvkEmptyArray then
      { Declared with no scopes. }
    else if (Entry.Kind = cvkString) and Entry.InArray then
    begin
      if Trim(Entry.Value) = '' then
        raise TParseError.CreateFmt(
          '%s: "%s" has an empty scope', [Location, Entry.Key]);
      Scope := ResolvePermissionScope(Capability, Entry.Value,
        ExtractFilePath(Location));
      ValidateScope(Location, Entry.Key, Capability, Scope, Entry.Value);
      if not Overridden then
      begin
        SetLength(Scopes.Scopes, Length(Scopes.Scopes) + 1);
        Scopes.Scopes[High(Scopes.Scopes)] := Scope;
      end;
    end
    else
      raise EGocciaConfigPermissionError.CreateFmt(
        '%s: "%s" must be true, false, or an array of strings',
        [Location, Entry.Key]);

    if Overridden then
      Continue;
    if Allow then
      Result.Allow[Capability] := Scopes
    else
      Result.Deny[Capability] := Scopes;
  end;
end;

function DescribeCapabilities(const ACapabilities: TGocciaHonoredCapabilities):
  string;
var
  Capability: TGocciaCapability;
  Names: array of string;
  I: Integer;
begin
  Names := nil;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if Capability in ACapabilities then
    begin
      SetLength(Names, Length(Names) + 1);
      Names[High(Names)] := CapabilityName(Capability);
    end;
  if Length(Names) = 0 then
    Exit('no capability flags');
  Result := Names[0];
  for I := 1 to High(Names) do
    if I = High(Names) then
    begin
      if Length(Names) > 2 then
        Result := Result + ',';
      Result := Result + ' and ' + Names[I];
    end
    else
      Result := Result + ', ' + Names[I];
end;

function UnsupportedRequestWarnings(
  const ARequest: TGocciaConfigPermissionRequest;
  const AHonored: TGocciaHonoredCapabilities;
  const AProgramName: string;
  const AHonorsUnsafe, AHonorsSandbox: Boolean): TGocciaCapabilityScopes;
var
  Warnings: TGocciaCapabilityScopes;

  procedure AddWarning(const AKey: string);
  begin
    SetLength(Warnings, Length(Warnings) + 1);
    Warnings[High(Warnings)] := Format(
      'requests %s, which %s cannot grant; ignoring it',
      [AKey, AProgramName]);
  end;

var
  Capability: TGocciaCapability;
  UnsafeRequest: TGocciaUnsafeRequest;
begin
  Warnings := nil;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if (not (Capability in AHonored)) and
       ARequest.Allow[Capability].RequestsAny then
      AddWarning(PermissionKeyName(True, Capability));
  if not AHonorsUnsafe then
    for UnsafeRequest := Low(TGocciaUnsafeRequest) to
      High(TGocciaUnsafeRequest) do
      if UnsafeRequest in ARequest.Unsafe then
        AddWarning(UNSAFE_REQUEST_KEYS[UnsafeRequest]);
  if (not AHonorsSandbox) and ARequest.Sandbox.Declared then
  begin
    SetLength(Warnings, Length(Warnings) + 1);
    Warnings[High(Warnings)] := Format(
      'declares a "%s" section, which %s does not use; ignoring it',
      [SANDBOX_CONFIG_KEY, AProgramName]);
  end;
  Result := Warnings;
end;

{ ── Normalized block ──────────────────────────────────────────── }

const
  UNSAFE_BLOCK_KEY = 'unsafe';
  VERSION_BLOCK_KEY = 'version';
  JSON_TRUE = 'true';
  SCOPE_LIST_SEPARATOR = ', ';

function NormalizeScope(const ACapability: TGocciaCapability;
  const AScope: string): string;
begin
  Result := Trim(AScope);
  case ACapability of
    gcNet:
      Result := LowerCase(Result);
    gcImport:
      if SameText(Copy(Result, 1, Length(NODE_MODULES_CEILING_PREFIX)),
         NODE_MODULES_CEILING_PREFIX) then
        Result := NODE_MODULES_CEILING_PREFIX +
          Copy(Result, Length(NODE_MODULES_CEILING_PREFIX) + 1, MaxInt)
      else
        { `node_modules` and provider names are case-insensitive. }
        Result := LowerCase(Result);
  end;
end;

{ A list that sorts and compares in byte order. }
function CreateByteOrderList: TStringList;
begin
  Result := TStringList.Create;
  Result.UseLocale := False;
  Result.CaseSensitive := True;
end;

{ Normalized scopes in byte order, without duplicates. }
function SortedUniqueScopes(const ACapability: TGocciaCapability;
  const AScopes: TGocciaCapabilityScopes): TGocciaCapabilityScopes;
var
  List: TStringList;
  I: Integer;
begin
  List := CreateByteOrderList;
  try
    List.Sorted := True;
    List.Duplicates := dupIgnore;
    for I := 0 to High(AScopes) do
      List.Add(NormalizeScope(ACapability, AScopes[I]));
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I];
  finally
    List.Free;
  end;
end;

function ScopesJSON(const ACapability: TGocciaCapability;
  const AScopes: TGocciaPermissionScopes): string;
var
  Scopes: TGocciaCapabilityScopes;
  I: Integer;
begin
  if AScopes.Unscoped then
    Exit(JSON_TRUE);
  Scopes := SortedUniqueScopes(ACapability, AScopes.Scopes);
  Result := '[';
  for I := 0 to High(Scopes) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + QuoteJSONString(Scopes[I]);
  end;
  Result := Result + ']';
end;

{ The sandbox section as a canonical JSON object. Inputs keep their
  declaration order and duplicates, because a later input may overwrite an
  earlier one in the sandbox, so reordering them changes the run. }
function SandboxBlockJSON(const ASandbox: TGocciaSandboxRequest): string;

  procedure AddMember(const AKey, AValue: string);
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + QuoteJSONString(AKey) + ':' + AValue;
  end;

  function InputsJSON(const AReadWrite: Boolean): string;
  var
    I: Integer;
    Text: string;
  begin
    Result := '';
    for I := 0 to High(ASandbox.Inputs) do
    begin
      if ASandbox.Inputs[I].ReadWrite <> AReadWrite then
        Continue;
      Text := ASandbox.Inputs[I].HostPath;
      if ASandbox.Inputs[I].SandboxPath <> '' then
        Text := Text + '=' + ASandbox.Inputs[I].SandboxPath;
      if Result <> '' then
        Result := Result + ',';
      Result := Result + QuoteJSONString(Text);
    end;
    if Result <> '' then
      Result := '[' + Result + ']';
  end;

var
  Inputs: string;
begin
  Result := '';
  Inputs := InputsJSON(False);
  if Inputs <> '' then
    AddMember(SANDBOX_COPY_KEY, Inputs);
  Inputs := InputsJSON(True);
  if Inputs <> '' then
    AddMember(SANDBOX_COPY_READ_WRITE_KEY, Inputs);
  if ASandbox.Diff = SANDBOX_DIFF_DEFAULT then
    AddMember(SANDBOX_DIFF_KEY, JSON_TRUE)
  else if ASandbox.Diff <> '' then
    AddMember(SANDBOX_DIFF_KEY, QuoteJSONString(ASandbox.Diff));
  if ASandbox.DiffFile <> '' then
    AddMember(SANDBOX_DIFF_FILE_KEY, QuoteJSONString(ASandbox.DiffFile));
  if ASandbox.Entry <> '' then
    AddMember(SANDBOX_ENTRY_KEY, QuoteJSONString(ASandbox.Entry));
  Result := '{' + Result + '}';
end;

function NormalizedPermissionBlock(
  const ARequest: TGocciaConfigPermissionRequest): string;
var
  Keys, Values: TStringList;
  Allow: Boolean;
  Capability: TGocciaCapability;
  Scopes: TGocciaPermissionScopes;
  UnsafeRequest: TGocciaUnsafeRequest;
  Permissions, Unsafe: string;
  I: Integer;
begin
  Permissions := '';
  Unsafe := '';
  { Keys carry an index into Values, so sorting the keys sorts the members. }
  Keys := CreateByteOrderList;
  Values := TStringList.Create;
  try
    for Allow := True downto False do
      for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
      begin
        if Allow then
          Scopes := ARequest.Allow[Capability]
        else
          Scopes := ARequest.Deny[Capability];
        if Scopes.RequestsAny then
          Keys.AddObject(PermissionKeyName(Allow, Capability),
            TObject(NativeInt(Values.Add(ScopesJSON(Capability, Scopes)))));
      end;
    Keys.Sort;
    for I := 0 to Keys.Count - 1 do
    begin
      if Permissions <> '' then
        Permissions := Permissions + ',';
      Permissions := Permissions + QuoteJSONString(Keys[I]) + ':' +
        Values[NativeInt(Keys.Objects[I])];
    end;

    Keys.Clear;
    for UnsafeRequest := Low(TGocciaUnsafeRequest) to
      High(TGocciaUnsafeRequest) do
      if UnsafeRequest in ARequest.Unsafe then
        Keys.Add(UNSAFE_REQUEST_KEYS[UnsafeRequest]);
    Keys.Sort;
    for I := 0 to Keys.Count - 1 do
    begin
      if Unsafe <> '' then
        Unsafe := Unsafe + ',';
      Unsafe := Unsafe + QuoteJSONString(Keys[I]) + ':' + JSON_TRUE;
    end;
  finally
    Values.Free;
    Keys.Free;
  end;

  Result := '{';
  if Permissions <> '' then
    Result := Result + QuoteJSONString(PERMISSIONS_CONFIG_KEY) + ':{' +
      Permissions + '},';
  if ARequest.Sandbox.Declared then
    Result := Result + QuoteJSONString(SANDBOX_CONFIG_KEY) + ':' +
      SandboxBlockJSON(ARequest.Sandbox) + ',';
  if Unsafe <> '' then
    Result := Result + QuoteJSONString(UNSAFE_BLOCK_KEY) + ':{' + Unsafe +
      '},';
  Result := Result + QuoteJSONString(VERSION_BLOCK_KEY) + ':' +
    IntToStr(PERMISSION_BLOCK_VERSION) + '}';
end;

function PermissionBlockHash(
  const ARequest: TGocciaConfigPermissionRequest): string;
begin
  Result := SHA256Hex(EncodeUTF8WithReplacement(
    NormalizedPermissionBlock(ARequest)));
end;

type
  { Reads a normalized block back into one description line per key. }
  TPermissionBlockLineParser = class(TAbstractJSONParser)
  private
    FLines: TStringList;
    FDepth: Integer;
    FSection: string;
    FKey: string;
    FScopes: string;
    FInScopes: Boolean;
    { Lines added since the sandbox object opened, to describe an empty
      one. }
    FSandboxLines: Integer;
    procedure AddLine(const AValue: string);
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
  public
    function Parse(const AText: string): TGocciaCapabilityScopes;
  end;

{ What an unscoped key covers, for a description line. }
function UnscopedDescription(const AKey: string): string;
var
  Allow: Boolean;
  Capability: TGocciaCapability;
begin
  if not TryParsePermissionKey(AKey, Allow, Capability) then
    Exit(JSON_TRUE);
  case Capability of
    gcRead:
      Result := 'any path';
    gcNet:
      if Allow then
        Result := 'any public host'
      else
        Result := 'any host';
    gcFFI:
      Result := 'any library';
  else
    Result := JSON_TRUE;
  end;
end;

procedure TPermissionBlockLineParser.AddLine(const AValue: string);
begin
  if FSection = SANDBOX_CONFIG_KEY then
  begin
    FLines.Add(SANDBOX_CONFIG_KEY + '.' + FKey + ': ' + AValue);
    Inc(FSandboxLines);
  end
  else
    FLines.Add(FKey + ': ' + AValue);
end;

procedure TPermissionBlockLineParser.OnNull;
begin
end;

procedure TPermissionBlockLineParser.OnBoolean(const AValue: Boolean);
begin
  if (FDepth <> 2) or not AValue then
    Exit;
  if FSection = PERMISSIONS_CONFIG_KEY then
    AddLine(UnscopedDescription(FKey))
  else if (FSection = UNSAFE_BLOCK_KEY) or
    (FSection = SANDBOX_CONFIG_KEY) then
    AddLine(JSON_TRUE);
end;

procedure TPermissionBlockLineParser.OnString(const AValue: string);
begin
  if (FDepth = 2) and (FSection = SANDBOX_CONFIG_KEY) then
  begin
    AddLine(AValue);
    Exit;
  end;
  if not FInScopes then
    Exit;
  if FScopes <> '' then
    FScopes := FScopes + SCOPE_LIST_SEPARATOR;
  FScopes := FScopes + AValue;
end;

procedure TPermissionBlockLineParser.OnInteger(const AValue: Int64);
begin
end;

procedure TPermissionBlockLineParser.OnFloat(const AValue: Double);
begin
end;

procedure TPermissionBlockLineParser.OnBeginObject;
begin
  Inc(FDepth);
  if (FDepth = 2) and (FSection = SANDBOX_CONFIG_KEY) then
    FSandboxLines := 0;
end;

procedure TPermissionBlockLineParser.OnObjectKey(const AKey: string);
begin
  if FDepth = 1 then
    FSection := AKey
  else if FDepth = 2 then
    FKey := AKey;
end;

procedure TPermissionBlockLineParser.OnEndObject;
begin
  if (FDepth = 2) and (FSection = SANDBOX_CONFIG_KEY) and
     (FSandboxLines = 0) then
    FLines.Add(SANDBOX_CONFIG_KEY + ': no inputs (sandbox mode only)');
  Dec(FDepth);
end;

procedure TPermissionBlockLineParser.OnBeginArray;
begin
  Inc(FDepth);
  if (FDepth = 3) and ((FSection = PERMISSIONS_CONFIG_KEY) or
     (FSection = SANDBOX_CONFIG_KEY)) then
  begin
    FInScopes := True;
    FScopes := '';
  end;
end;

procedure TPermissionBlockLineParser.OnEndArray;
begin
  if FInScopes and (FDepth = 3) then
  begin
    FInScopes := False;
    AddLine(FScopes);
  end;
  Dec(FDepth);
end;

function TPermissionBlockLineParser.Parse(
  const AText: string): TGocciaCapabilityScopes;
var
  I: Integer;
begin
  Result := nil;
  FLines := TStringList.Create;
  try
    FDepth := 0;
    FInScopes := False;
    try
      DoParse(AText);
    except
      on E: EJSONParseError do
        FLines.Clear;
    end;
    SetLength(Result, FLines.Count);
    for I := 0 to FLines.Count - 1 do
      Result[I] := FLines[I];
  finally
    FreeAndNil(FLines);
  end;
end;

function PermissionBlockLines(const ABlockJSON: string): TGocciaCapabilityScopes;
var
  Parser: TPermissionBlockLineParser;
begin
  Parser := TPermissionBlockLineParser.Create;
  try
    Result := Parser.Parse(ABlockJSON);
  finally
    Parser.Free;
  end;
end;

function DescribePermissionRequest(
  const ARequest: TGocciaConfigPermissionRequest;
  const AIndent: string): string;
var
  Lines: TGocciaCapabilityScopes;
  I: Integer;
begin
  Result := '';
  Lines := PermissionBlockLines(NormalizedPermissionBlock(ARequest));
  for I := 0 to High(Lines) do
    Result := Result + AIndent + Lines[I] + sLineBreak;
end;

function ContainsLine(const ALines: TGocciaCapabilityScopes;
  const ALine: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(ALines) do
    if ALines[I] = ALine then
      Exit(True);
  Result := False;
end;

function DescribePermissionChange(const APreviousBlockJSON: string;
  const ACurrent: TGocciaConfigPermissionRequest;
  const AIndent: string): string;
var
  Previous, Current: TGocciaCapabilityScopes;
  MarkerIndent: string;
  I: Integer;
begin
  Previous := PermissionBlockLines(APreviousBlockJSON);
  Current := PermissionBlockLines(NormalizedPermissionBlock(ACurrent));
  MarkerIndent := Copy(AIndent, 1, Length(AIndent) - 2);
  Result := '';
  for I := 0 to High(Previous) do
    if not ContainsLine(Current, Previous[I]) then
      Result := Result + MarkerIndent + '- ' + Previous[I] + sLineBreak;
  for I := 0 to High(Current) do
    if ContainsLine(Previous, Current[I]) then
      Result := Result + AIndent + Current[I] + sLineBreak
    else
      Result := Result + MarkerIndent + '+ ' + Current[I] + sLineBreak;
end;

end.
