unit Goccia.CLI.Permissions;

{ A config file's `permissions` block, read into a request (ADR 0122).

  A block lists `allow-<cap>` and `deny-<cap>` keys for read, net, ffi, and
  import. Each value is `true` (every scope), `false` (absent; lets a child
  config cancel a base), or an array of scope strings. Relative path scopes,
  and the directory of `node_modules=<dir>`, resolve against the directory of
  the file that declares them, so a scope means the same place wherever the
  command runs from. With `extends`, the child overrides the base key by key.

  A request is not a grant: the application decides whether to accept it. }

{$I Goccia.inc}

interface

uses
  CLI.ConfigFile,
  CLI.Options,

  Goccia.Capabilities;

type
  TGocciaHonoredCapabilities = set of TGocciaCapability;

  TGocciaPermissionScopes = record
    { The key appears in the effective config (even as false or []). }
    Declared: Boolean;
    Unscoped: Boolean;
    Scopes: TGocciaCapabilityScopes;
    { The file whose value is in effect. }
    SourcePath: string;
    function RequestsAny: Boolean;
  end;

  TGocciaConfigPermissionRequest = record
    ConfigPath: string;
    Allow: array[TGocciaCapability] of TGocciaPermissionScopes;
    Deny: array[TGocciaCapability] of TGocciaPermissionScopes;
    class function Empty: TGocciaConfigPermissionRequest; static;
    { True when some allow-* key asks for something. A deny-only block asks
      for nothing. }
    function RequestsGrants: Boolean;
    function RequestsHonoredGrants(
      const AHonored: TGocciaHonoredCapabilities): Boolean;
  end;

  { A malformed permissions block: an unknown key or a value of the wrong
    shape. A usage error, so the run exits 2. }
  EGocciaConfigPermissionError = class(TCLIUsageError);

const
  PERMISSIONS_CONFIG_KEY = 'permissions';
  { What an import scope must name, for the missing-scope errors. }
  IMPORT_SCOPE_REQUIREMENT =
    'node_modules[=<dir>] or a provider such as github';
  ALL_CAPABILITIES: TGocciaHonoredCapabilities = [gcRead, gcNet, gcFFI,
    gcImport];

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
  not include: `Warning: <config> requests allow-<cap>, which <Program>
  cannot grant; ignoring it`. }
function UnsupportedRequestWarnings(
  const ARequest: TGocciaConfigPermissionRequest;
  const AHonored: TGocciaHonoredCapabilities;
  const AProgramName: string): TGocciaCapabilityScopes;

{ The parenthesized advice of an invalid-scope error for ACapability. }
function PermissionScopeHint(const ACapability: TGocciaCapability): string;

{ `read, net, and ffi`; `no capability flags` for an empty set. }
function DescribeCapabilities(const ACapabilities: TGocciaHonoredCapabilities):
  string;

implementation

uses
  SysUtils,

  FileUtils;

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
  Result := RequestsHonoredGrants(ALL_CAPABILITIES);
end;

function TGocciaConfigPermissionRequest.RequestsHonoredGrants(
  const AHonored: TGocciaHonoredCapabilities): Boolean;
var
  Capability: TGocciaCapability;
begin
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

function ReadConfigPermissionRequest(const AEntries: TConfigEntryArray;
  const AConfigPath: string): TGocciaConfigPermissionRequest;
var
  I: Integer;
  Allow: Boolean;
  Capability: TGocciaCapability;
  SubKey, Location, Scope: string;
  Entry: TConfigEntry;
  Scopes: TGocciaPermissionScopes;
  Overridden: Boolean;
begin
  Result := TGocciaConfigPermissionRequest.Empty;
  Result.ConfigPath := AConfigPath;
  for I := 0 to High(AEntries) do
  begin
    Entry := AEntries[I];
    Location := Entry.SourcePath;
    if Location = '' then
      Location := AConfigPath;

    if Entry.Key = PERMISSIONS_CONFIG_KEY then
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
  const AProgramName: string): TGocciaCapabilityScopes;
var
  Capability: TGocciaCapability;
begin
  Result := nil;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if (not (Capability in AHonored)) and
       ARequest.Allow[Capability].RequestsAny then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Format(
        'Warning: %s requests %s, which %s cannot grant; ignoring it',
        [ARequest.ConfigPath, PermissionKeyName(True, Capability),
         AProgramName]);
    end;
end;

end.
