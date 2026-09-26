unit Goccia.Capabilities;

{ The engine-owned capability set (ADR 0122).

  A capability set says which host resources an engine may reach beyond the
  process: host files (`read`), the network (`net`), native libraries (`ffi`),
  and non-local module sources (`import`). It is an immutable value: every
  builder returns a new set and deep-copies the rules it carries, so a set
  handed to an engine can never change underneath it, and no caller can reach
  the rule arrays inside it.

  A set is a stack of layers. The root layer is what a host granted; each
  nested context (a ShadowRealm, a sandbox runScript child, a narrowed embedder
  context) appends a layer through Narrow. A request is allowed only when every
  layer allows it and no layer denies it, so a narrowed set can never allow
  more than its parent. Within a layer, and across layers, a deny always wins
  over an allow regardless of the order they were added in.

  A zero-initialized value, and None, grant nothing. }

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaCapability = (gcRead, gcNet, gcFFI, gcImport);

  EGocciaCapabilityScopeError = class(Exception);

  TGocciaCapabilityScopes = array of string;

  { One capability's rules inside one layer. Implementation detail of
    TGocciaCapabilities; declared here only because the record field needs a
    type. Nothing outside this unit can reach an instance. }
  TGocciaCapabilityRule = record
    AllowAll: Boolean;
    DenyAll: Boolean;
    AllowScopes: TGocciaCapabilityScopes;
    DenyScopes: TGocciaCapabilityScopes;
  end;

  TGocciaCapabilityLayer = record
    Rules: array[TGocciaCapability] of TGocciaCapabilityRule;
  end;

  TGocciaCapabilityLayers = array of TGocciaCapabilityLayer;

  TGocciaNetHostVerdict = (nhvAllowed, nhvDenied, nhvNotAllowed,
    nhvPrivateNotNamed);

  TGocciaCapabilities = record
  private
    FLayers: TGocciaCapabilityLayers;
    function CopyWithScope(const ACapability: TGocciaCapability;
      const AScope: string; const AAllow: Boolean): TGocciaCapabilities;
  public
    { Grants nothing. The default for an engine created without a set. }
    class function None: TGocciaCapabilities; static;
    { Grants every capability, including private network ranges. Intended for
      tests and fully trusted hosts. }
    class function Unrestricted: TGocciaCapabilities; static;

    { Returns a copy with AScope allowed (or denied) for ACapability in the
      innermost layer. An empty scope means every scope of that capability.
      Raises EGocciaCapabilityScopeError for a malformed or relative scope. }
    function Allow(const ACapability: TGocciaCapability;
      const AScope: string = ''): TGocciaCapabilities;
    function Deny(const ACapability: TGocciaCapability;
      const AScope: string = ''): TGocciaCapabilities;

    { Returns a copy with AChild's layers appended. The result never allows a
      request this set denies. A child with no layers narrows to nothing. }
    function Narrow(const AChild: TGocciaCapabilities): TGocciaCapabilities;

    { True when some request of this capability could be allowed: every layer
      allows at least one scope and none denies the capability outright. }
    function Grants(const ACapability: TGocciaCapability): Boolean;

    { True when every layer allows the capability unscoped and no layer denies
      any scope of it. The only answer for a request that no scope can
      describe, such as a library name the platform loader searches for. }
    function AllowsUnscoped(const ACapability: TGocciaCapability): Boolean;

    { True when some layer denies the capability outright (an unscoped deny).
      For read this also removes the module-graph exemption. }
    function DeniesAll(const ACapability: TGocciaCapability): Boolean;

    { read/ffi: true when some layer denies APath, outright or through a deny
      scope covering it. Deny wins over grants and exemptions alike. }
    function DeniesPath(const ACapability: TGocciaCapability;
      const APath: string): Boolean;

    { Generic request query. read/ffi take a path, net takes `host`,
      `host:port`, or `[v6]:port`, import takes `node_modules` or a provider
      name. }
    function Allows(const ACapability: TGocciaCapability;
      const ARequest: string): Boolean;

    { read/ffi: APath is canonicalized (symlinks resolved) before matching. }
    function AllowsPath(const ACapability: TGocciaCapability;
      const APath: string): Boolean;

    { read/ffi: APath is already canonical — for example the path the kernel
      reports for a file descriptor already opened — and is matched as
      written, without touching the filesystem again. }
    function AllowsCanonicalPath(const ACapability: TGocciaCapability;
      const APath: string): Boolean;

    { net, before name resolution. An IP-literal host is judged as the
      destination address it names. A host name allowed only through the
      `private` scope passes provisionally: `private` grants private
      destinations, and whether the name is one is known only once it
      resolves (see AllowsNetAddress). }
    function AllowsNetHost(const AHost: string; const APort: Integer): Boolean;

    { net, after name resolution: the request's host and port with the address
      the host resolved to. Private, loopback, and link-local addresses are
      allowed only when every layer names them, through the `private` scope or
      an explicit IP/CIDR scope that covers the address; a host allow alone,
      even an unscoped one, does not. A public address needs a host allow. A
      deny of `private` or of a covering IP/CIDR refuses the address whatever
      allows it. }
    { The verdict AllowsNetHost is based on: allowed, denied by a deny rule,
      a private address nothing names, or not allowed by any rule. }
    function NetHostVerdict(const AHost: string;
      const APort: Integer): TGocciaNetHostVerdict;

    { When a net deny refuses AHost:APort, True with the deny scope that
      matched it ('' for an unscoped deny), for host-side reports. }
    function NetDenyScope(const AHost: string; const APort: Integer;
      out AScope: string): Boolean;

    { Why AllowsNetHost refuses AHost:APort, for audit reasons; empty when it
      allows it. Host-side text: never shown to the guest. }
    function ExplainNetHostDenial(const AHost: string;
      const APort: Integer): string;

    function AllowsNetAddress(const AHost: string; const APort: Integer;
      const AAddress: string): Boolean;

    { import: whether a bare specifier imported from AImportingDirectory may be
      resolved against node_modules, and the highest directory the ancestor
      walk may reach (empty = unbounded). }
    function NodeModulesCeiling(const AImportingDirectory: string;
      out ACeiling: string): Boolean;

    { import: true when node_modules resolution for AImportingDirectory is
      denied outright, as opposed to merely never granted. }
    function DeniesNodeModules(const AImportingDirectory: string): Boolean;

    { import: whether provider imports from AProvider (e.g. `github`) are
      allowed. Provider resolution itself is not implemented yet. }
    function AllowsProvider(const AProvider: string): Boolean;

    function LayerCount: Integer;
    function ToJSON: string;
  end;

const
  NET_PRIVATE_SCOPE = 'private';
  IMPORT_NODE_MODULES_SCOPE = 'node_modules';

function CapabilityName(const ACapability: TGocciaCapability): string;
function TryParseCapabilityName(const AName: string;
  out ACapability: TGocciaCapability): Boolean;

{ Absolute, symlink-resolved spelling of APath without a trailing separator.
  The deepest existing ancestor is canonicalized and the rest appended, so a
  path that does not exist yet still compares against canonical scopes. }
function CanonicalCapabilityPath(const APath: string): string;

{ True when APath equals AScope or lives beneath it. Both must already be
  canonical. /a/b does not contain /a/bc. }
function IsPathWithinScope(const APath, AScope: string): Boolean;

implementation

uses
  FileUtils,
  NetworkAddress,

  Goccia.JSON.Utils;

const
  { Port argument that matches a scope whatever port it names. }
  NET_ANY_PORT = -1;

type
  TGocciaNetScopeKind = (nskPrivate, nskHost, nskWildcard, nskAddress,
    nskCIDR);

  TGocciaNetScope = record
    Kind: TGocciaNetScopeKind;
    Host: string;
    Address: TNetworkAddress;
    PrefixLength: Integer;
    Port: Integer;
  end;

  TGocciaImportScopeKind = (iskNodeModules, iskProvider);

  TGocciaImportScope = record
    Kind: TGocciaImportScopeKind;
    Ceiling: string;
    Provider: string;
  end;

const
  CAPABILITY_NAMES: array[TGocciaCapability] of string = (
    'read', 'net', 'ffi', 'import');
  NODE_MODULES_CEILING_SEPARATOR = '=';
  MAX_PORT = 65535;

function CapabilityName(const ACapability: TGocciaCapability): string;
begin
  Result := CAPABILITY_NAMES[ACapability];
end;

function TryParseCapabilityName(const AName: string;
  out ACapability: TGocciaCapability): Boolean;
var
  Capability: TGocciaCapability;
begin
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    if SameText(AName, CAPABILITY_NAMES[Capability]) then
    begin
      ACapability := Capability;
      Exit(True);
    end;
  ACapability := gcRead;
  Result := False;
end;

{ ── paths ─────────────────────────────────────────────────────── }

function SamePathText(const A, B: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := SameText(A, B);
  {$ELSE}
  Result := A = B;
  {$ENDIF}
end;

function IsRootPath(const APath: string): Boolean;
begin
  Result := (APath <> '') and (ExtractFileDir(APath) = APath);
end;

function StripTrailingDelimiter(const APath: string): string;
begin
  Result := APath;
  while (Length(Result) > 1) and (Result[Length(Result)] = PathDelim) and
    not IsRootPath(Result) do
    Delete(Result, Length(Result), 1);
end;

function CanonicalCapabilityPath(const APath: string): string;
var
  Expanded, Parent, Canonical, Suffix: string;
begin
  Result := '';
  if APath = '' then
    Exit;
  Expanded := StripTrailingDelimiter(ExpandHostFileName(APath));
  Suffix := '';
  Parent := Expanded;
  while Parent <> '' do
  begin
    Canonical := CanonicalHostPath(Parent);
    if Canonical <> '' then
    begin
      Canonical := StripTrailingDelimiter(Canonical);
      if Suffix = '' then
        Exit(Canonical);
      if Canonical[Length(Canonical)] = PathDelim then
        Exit(Canonical + Suffix);
      Exit(Canonical + PathDelim + Suffix);
    end;
    if IsRootPath(Parent) then
      Break;
    if Suffix = '' then
      Suffix := ExtractFileName(Parent)
    else
      Suffix := ExtractFileName(Parent) + PathDelim + Suffix;
    Canonical := ExtractFileDir(Parent);
    if (Canonical = '') or SamePathText(Canonical, Parent) then
      Break;
    Parent := Canonical;
  end;
  { No ancestor could be canonicalized (the Lakon lane, or an unreadable
    root): the lexical spelling is the best answer available. }
  Result := Expanded;
end;

function IsPathWithinScope(const APath, AScope: string): Boolean;
var
  Prefix: string;
begin
  if (APath = '') or (AScope = '') then
    Exit(False);
  if SamePathText(APath, AScope) then
    Exit(True);
  if AScope[Length(AScope)] = PathDelim then
    Prefix := AScope
  else
    Prefix := AScope + PathDelim;
  Result := (Length(APath) > Length(Prefix)) and
    SamePathText(Copy(APath, 1, Length(Prefix)), Prefix);
end;

function NormalizePathScope(const ACapability: TGocciaCapability;
  const AScope: string): string;
begin
  if not IsAbsoluteHostPath(AScope) then
    raise EGocciaCapabilityScopeError.CreateFmt(
      '%s scope must be an absolute path: %s',
      [CapabilityName(ACapability), AScope]);
  Result := CanonicalCapabilityPath(AScope);
end;

{ ── net scopes ────────────────────────────────────────────────── }

function IsValidHostName(const AHost: string): Boolean;
var
  I: Integer;
begin
  if (AHost = '') or (AHost[1] = '.') or (AHost[Length(AHost)] = '.') then
    Exit(False);
  for I := 1 to Length(AHost) do
    case AHost[I] of
      'a'..'z', '0'..'9', '-', '.', '_':
        ;
    else
      Exit(False);
    end;
  Result := Pos('..', AHost) = 0;
end;

function TryParsePort(const AText: string; out APort: Integer): Boolean;
begin
  Result := TryStrToInt(AText, APort) and (APort >= 1) and
    (APort <= MAX_PORT) and (Pos('+', AText) = 0) and (Pos('-', AText) = 0);
end;

{ A fully qualified name's trailing dot names the same host, so one is
  dropped before matching; otherwise `host.` would slip past a deny on
  `host`. }
function StripTrailingDot(const AHost: string): string;
begin
  Result := AHost;
  if (Length(Result) > 1) and (Result[Length(Result)] = '.') then
    Delete(Result, Length(Result), 1);
end;

function TryParseNetScope(const AScope: string;
  out ANetScope: TGocciaNetScope): Boolean;
var
  Text, HostPart, PortPart: string;
  CloseBracket, ColonPos, ColonCount, I: Integer;
begin
  Result := False;
  ANetScope := Default(TGocciaNetScope);
  Text := LowerCase(Trim(AScope));
  if Text = '' then
    Exit;

  if Text = NET_PRIVATE_SCOPE then
  begin
    ANetScope.Kind := nskPrivate;
    Exit(True);
  end;

  if Pos('/', Text) > 0 then
  begin
    ANetScope.Kind := nskCIDR;
    Exit(TryParseCIDR(Text, ANetScope.Address, ANetScope.PrefixLength));
  end;

  HostPart := Text;
  PortPart := '';
  if Text[1] = '[' then
  begin
    CloseBracket := Pos(']', Text);
    if CloseBracket < 3 then
      Exit;
    HostPart := Copy(Text, 2, CloseBracket - 2);
    if CloseBracket < Length(Text) then
    begin
      if Text[CloseBracket + 1] <> ':' then
        Exit;
      PortPart := Copy(Text, CloseBracket + 2, MaxInt);
      if PortPart = '' then
        Exit;
    end;
  end
  else
  begin
    ColonCount := 0;
    for I := 1 to Length(Text) do
      if Text[I] = ':' then
        Inc(ColonCount);
    if ColonCount = 1 then
    begin
      ColonPos := Pos(':', Text);
      HostPart := Copy(Text, 1, ColonPos - 1);
      PortPart := Copy(Text, ColonPos + 1, MaxInt);
      if PortPart = '' then
        Exit;
    end;
  end;

  if PortPart <> '' then
  begin
    if not TryParsePort(PortPart, ANetScope.Port) then
      Exit;
  end;

  { One trailing dot is dropped for an IP literal as for a name, matching
    NormalizeRequestHost on the request side. }
  if TryParseIPAddress(StripTrailingDot(HostPart), ANetScope.Address) then
  begin
    ANetScope.Kind := nskAddress;
    Exit(True);
  end;

  if Pos(':', HostPart) > 0 then
    Exit;

  HostPart := StripTrailingDot(HostPart);
  if Copy(HostPart, 1, 2) = '*.' then
  begin
    ANetScope.Kind := nskWildcard;
    ANetScope.Host := Copy(HostPart, 3, MaxInt);
    Exit(IsValidHostName(ANetScope.Host));
  end;

  ANetScope.Kind := nskHost;
  ANetScope.Host := HostPart;
  Result := IsValidHostName(HostPart);
end;

function NormalizeNetScope(const AScope: string): string;
var
  NetScope: TGocciaNetScope;
begin
  if not TryParseNetScope(AScope, NetScope) then
    raise EGocciaCapabilityScopeError.CreateFmt(
      'net scope is not a host, host:port, *.domain, IP, CIDR, or private: %s',
      [AScope]);
  Result := LowerCase(Trim(AScope));
end;

function NormalizeRequestHost(const AHost: string): string;
begin
  Result := LowerCase(Trim(AHost));
  if (Length(Result) >= 2) and (Result[1] = '[') and
     (Result[Length(Result)] = ']') then
    Result := Copy(Result, 2, Length(Result) - 2);
  Result := StripTrailingDot(Result);
end;

function ScopeCoversExactAddress(const ANetScope: TGocciaNetScope;
  const AAddress: TNetworkAddress): Boolean;
begin
  case ANetScope.Kind of
    nskAddress:
      Result := AddressesEqual(AAddress, ANetScope.Address);
    nskCIDR:
      Result := IsAddressInNetwork(AAddress, ANetScope.Address,
        ANetScope.PrefixLength);
  else
    Result := False;
  end;
end;

{ An IP or CIDR scope covers an address. With AForDeny it also covers the
  IPv4 host a NAT64 (64:ff9b::/96) or 6to4 (2002::/16) address reaches, so a
  deny cannot be sidestepped through those spellings. An allow does not
  extend that way: a 6to4 prefix names a relay site, not the IPv4 host, and
  NAT64 follows the same rule so both translations behave alike. }
function NetScopeCoversAddress(const ANetScope: TGocciaNetScope;
  const AAddress: TNetworkAddress; const AForDeny: Boolean): Boolean;
var
  Translated: TNetworkAddress;
begin
  Result := ScopeCoversExactAddress(ANetScope, AAddress) or
    (AForDeny and TryGetTranslatedIPv4(AAddress, Translated) and
     ScopeCoversExactAddress(ANetScope, Translated));
end;

{ Whether a non-private scope names this destination. APort of zero means the
  request did not state one, which only an unported scope can match. }
function NetScopeMatchesHost(const ANetScope: TGocciaNetScope;
  const AHost: string; const AHostIsAddress: Boolean;
  const AHostAddress: TNetworkAddress; const APort: Integer;
  const AForDeny: Boolean): Boolean;
var
  Suffix: string;
begin
  if (ANetScope.Port <> 0) and (APort <> NET_ANY_PORT) and
     (ANetScope.Port <> APort) then
    Exit(False);
  case ANetScope.Kind of
    nskHost:
      Result := (not AHostIsAddress) and (AHost = ANetScope.Host);
    nskWildcard:
      begin
        Suffix := '.' + ANetScope.Host;
        Result := (not AHostIsAddress) and (Length(AHost) > Length(Suffix)) and
          (Copy(AHost, Length(AHost) - Length(Suffix) + 1, MaxInt) = Suffix);
      end;
    nskAddress, nskCIDR:
      Result := AHostIsAddress and
        NetScopeCoversAddress(ANetScope, AHostAddress, AForDeny);
  else
    Result := False;
  end;
end;

{ ── import scopes ─────────────────────────────────────────────── }

function TryParseImportScope(const AScope: string;
  out AImportScope: TGocciaImportScope): Boolean;
var
  Text, Prefix: string;
begin
  AImportScope := Default(TGocciaImportScope);
  Text := Trim(AScope);
  Result := False;
  if Text = '' then
    Exit;
  if SameText(Text, IMPORT_NODE_MODULES_SCOPE) then
  begin
    AImportScope.Kind := iskNodeModules;
    Exit(True);
  end;
  Prefix := IMPORT_NODE_MODULES_SCOPE + NODE_MODULES_CEILING_SEPARATOR;
  if SameText(Copy(Text, 1, Length(Prefix)), Prefix) then
  begin
    AImportScope.Kind := iskNodeModules;
    AImportScope.Ceiling := Copy(Text, Length(Prefix) + 1, MaxInt);
    Exit(AImportScope.Ceiling <> '');
  end;
  AImportScope.Kind := iskProvider;
  AImportScope.Provider := LowerCase(Text);
  Result := IsValidHostName(AImportScope.Provider);
end;

{ node_modules ceilings stay *expanded* rather than canonical: the ancestor
  walk in Goccia.Modules.NodeResolution compares expanded spellings, and a
  ceiling reached through a symlink fails closed (ADR 0111). }
function NormalizeImportScope(const AScope: string): string;
var
  ImportScope: TGocciaImportScope;
begin
  if not TryParseImportScope(AScope, ImportScope) then
    raise EGocciaCapabilityScopeError.CreateFmt(
      'import scope is not node_modules, node_modules=<dir>, or a provider: %s',
      [AScope]);
  case ImportScope.Kind of
    iskNodeModules:
      begin
        if ImportScope.Ceiling = '' then
          Exit(IMPORT_NODE_MODULES_SCOPE);
        if not IsAbsoluteHostPath(ImportScope.Ceiling) then
          raise EGocciaCapabilityScopeError.CreateFmt(
            'import node_modules ceiling must be an absolute path: %s',
            [ImportScope.Ceiling]);
        Result := IMPORT_NODE_MODULES_SCOPE + NODE_MODULES_CEILING_SEPARATOR +
          StripTrailingDelimiter(ExpandHostFileName(ImportScope.Ceiling));
      end;
  else
    Result := ImportScope.Provider;
  end;
end;

{ ── rule copying ──────────────────────────────────────────────── }

function CopyScopes(const AScopes: TGocciaCapabilityScopes):
  TGocciaCapabilityScopes;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AScopes));
  for I := 0 to High(AScopes) do
    Result[I] := AScopes[I];
end;

function CopyLayer(const ALayer: TGocciaCapabilityLayer):
  TGocciaCapabilityLayer;
var
  Capability: TGocciaCapability;
begin
  Result := Default(TGocciaCapabilityLayer);
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    Result.Rules[Capability].AllowAll := ALayer.Rules[Capability].AllowAll;
    Result.Rules[Capability].DenyAll := ALayer.Rules[Capability].DenyAll;
    Result.Rules[Capability].AllowScopes :=
      CopyScopes(ALayer.Rules[Capability].AllowScopes);
    Result.Rules[Capability].DenyScopes :=
      CopyScopes(ALayer.Rules[Capability].DenyScopes);
  end;
end;

function EmptyLayer: TGocciaCapabilityLayer;
var
  Capability: TGocciaCapability;
begin
  Result := Default(TGocciaCapabilityLayer);
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
  begin
    Result.Rules[Capability].AllowAll := False;
    Result.Rules[Capability].DenyAll := False;
    SetLength(Result.Rules[Capability].AllowScopes, 0);
    SetLength(Result.Rules[Capability].DenyScopes, 0);
  end;
end;

function CopyLayers(const ALayers: TGocciaCapabilityLayers):
  TGocciaCapabilityLayers;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(ALayers));
  for I := 0 to High(ALayers) do
    Result[I] := CopyLayer(ALayers[I]);
end;

procedure AppendScope(var AScopes: TGocciaCapabilityScopes;
  const AScope: string);
var
  I: Integer;
begin
  for I := 0 to High(AScopes) do
    if AScopes[I] = AScope then
      Exit;
  SetLength(AScopes, Length(AScopes) + 1);
  AScopes[High(AScopes)] := AScope;
end;

{ ── TGocciaCapabilities ───────────────────────────────────────── }

class function TGocciaCapabilities.None: TGocciaCapabilities;
begin
  Result := Default(TGocciaCapabilities);
  SetLength(Result.FLayers, 1);
  Result.FLayers[0] := EmptyLayer;
end;

class function TGocciaCapabilities.Unrestricted: TGocciaCapabilities;
var
  Capability: TGocciaCapability;
begin
  Result := None;
  for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    Result.FLayers[0].Rules[Capability].AllowAll := True;
  AppendScope(Result.FLayers[0].Rules[gcNet].AllowScopes, NET_PRIVATE_SCOPE);
end;

function TGocciaCapabilities.CopyWithScope(
  const ACapability: TGocciaCapability; const AScope: string;
  const AAllow: Boolean): TGocciaCapabilities;
var
  Normalized: string;
  LayerIndex: Integer;
begin
  Normalized := '';
  if Trim(AScope) <> '' then
    case ACapability of
      gcRead, gcFFI:
        Normalized := NormalizePathScope(ACapability, AScope);
      gcNet:
        Normalized := NormalizeNetScope(AScope);
      gcImport:
        Normalized := NormalizeImportScope(AScope);
    end;

  Result.FLayers := CopyLayers(FLayers);
  if Length(Result.FLayers) = 0 then
  begin
    SetLength(Result.FLayers, 1);
    Result.FLayers[0] := EmptyLayer;
  end;
  LayerIndex := High(Result.FLayers);
  if Normalized = '' then
  begin
    if AAllow then
      Result.FLayers[LayerIndex].Rules[ACapability].AllowAll := True
    else
      Result.FLayers[LayerIndex].Rules[ACapability].DenyAll := True;
  end
  else if AAllow then
    AppendScope(Result.FLayers[LayerIndex].Rules[ACapability].AllowScopes,
      Normalized)
  else
    AppendScope(Result.FLayers[LayerIndex].Rules[ACapability].DenyScopes,
      Normalized);
end;

function TGocciaCapabilities.Allow(const ACapability: TGocciaCapability;
  const AScope: string): TGocciaCapabilities;
begin
  Result := CopyWithScope(ACapability, AScope, True);
end;

function TGocciaCapabilities.Deny(const ACapability: TGocciaCapability;
  const AScope: string): TGocciaCapabilities;
begin
  Result := CopyWithScope(ACapability, AScope, False);
end;

function TGocciaCapabilities.Narrow(
  const AChild: TGocciaCapabilities): TGocciaCapabilities;
var
  Base, I: Integer;
  ChildLayers: TGocciaCapabilityLayers;
begin
  if Length(AChild.FLayers) = 0 then
  begin
    SetLength(ChildLayers, 1);
    ChildLayers[0] := EmptyLayer;
  end
  else
    ChildLayers := CopyLayers(AChild.FLayers);

  Result.FLayers := CopyLayers(FLayers);
  if Length(Result.FLayers) = 0 then
  begin
    SetLength(Result.FLayers, 1);
    Result.FLayers[0] := EmptyLayer;
  end;
  Base := Length(Result.FLayers);
  SetLength(Result.FLayers, Base + Length(ChildLayers));
  for I := 0 to High(ChildLayers) do
    Result.FLayers[Base + I] := ChildLayers[I];
end;

function TGocciaCapabilities.LayerCount: Integer;
begin
  Result := Length(FLayers);
end;

function TGocciaCapabilities.Grants(
  const ACapability: TGocciaCapability): Boolean;
var
  I: Integer;
  Rule: TGocciaCapabilityRule;
begin
  if Length(FLayers) = 0 then
    Exit(False);
  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[ACapability];
    if Rule.DenyAll or ((not Rule.AllowAll) and
       (Length(Rule.AllowScopes) = 0)) then
      Exit(False);
  end;
  Result := True;
end;

function TGocciaCapabilities.AllowsUnscoped(
  const ACapability: TGocciaCapability): Boolean;
var
  I: Integer;
  Rule: TGocciaCapabilityRule;
begin
  if Length(FLayers) = 0 then
    Exit(False);
  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[ACapability];
    if (not Rule.AllowAll) or Rule.DenyAll or
       (Length(Rule.DenyScopes) > 0) then
      Exit(False);
  end;
  Result := True;
end;

function TGocciaCapabilities.DeniesAll(
  const ACapability: TGocciaCapability): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FLayers) do
    if FLayers[I].Rules[ACapability].DenyAll then
      Exit(True);
  Result := False;
end;

function CanonicalPathRequest(const ACapability: TGocciaCapability;
  const APath: string): string;
begin
  if not (ACapability in [gcRead, gcFFI]) then
    raise EGocciaCapabilityScopeError.CreateFmt(
      '%s is not a path capability', [CapabilityName(ACapability)]);
  Result := CanonicalCapabilityPath(APath);
end;

function LayersDenyCanonicalPath(const ALayers: TGocciaCapabilityLayers;
  const ACapability: TGocciaCapability; const APath: string): Boolean;
var
  I, J: Integer;
  Rule: TGocciaCapabilityRule;
begin
  for I := 0 to High(ALayers) do
  begin
    Rule := ALayers[I].Rules[ACapability];
    if Rule.DenyAll then
      Exit(True);
    for J := 0 to High(Rule.DenyScopes) do
      if IsPathWithinScope(APath, Rule.DenyScopes[J]) then
        Exit(True);
  end;
  Result := False;
end;

function TGocciaCapabilities.DeniesPath(
  const ACapability: TGocciaCapability; const APath: string): Boolean;
var
  Path: string;
begin
  Path := CanonicalPathRequest(ACapability, APath);
  Result := (Path = '') or LayersDenyCanonicalPath(FLayers, ACapability, Path);
end;

function TGocciaCapabilities.AllowsPath(
  const ACapability: TGocciaCapability; const APath: string): Boolean;
begin
  Result := AllowsCanonicalPath(ACapability,
    CanonicalPathRequest(ACapability, APath));
end;

function TGocciaCapabilities.AllowsCanonicalPath(
  const ACapability: TGocciaCapability; const APath: string): Boolean;
var
  I, J: Integer;
  Path: string;
  LayerAllows: Boolean;
  Rule: TGocciaCapabilityRule;
begin
  if not (ACapability in [gcRead, gcFFI]) then
    raise EGocciaCapabilityScopeError.CreateFmt(
      '%s is not a path capability', [CapabilityName(ACapability)]);
  Path := APath;
  if (Length(FLayers) = 0) or (Path = '') then
    Exit(False);
  if LayersDenyCanonicalPath(FLayers, ACapability, Path) then
    Exit(False);

  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[ACapability];
    LayerAllows := Rule.AllowAll;
    J := 0;
    while (not LayerAllows) and (J <= High(Rule.AllowScopes)) do
    begin
      LayerAllows := IsPathWithinScope(Path, Rule.AllowScopes[J]);
      Inc(J);
    end;
    if not LayerAllows then
      Exit(False);
  end;
  Result := True;
end;

type
  { One net request as the rules see it: the host the URL names and, once
    known, the address the connection would reach. An IP-literal host is its
    own address before any lookup. }
  TGocciaNetRequest = record
    Host: string;
    HostIsAddress: Boolean;
    HostAddress: TNetworkAddress;
    Port: Integer;
    HasAddress: Boolean;
    Address: TNetworkAddress;
    AddressIsPrivate: Boolean;
  end;

function NetScopePortMatches(const ANetScope: TGocciaNetScope;
  const APort: Integer): Boolean;
begin
  Result := (ANetScope.Port = 0) or (APort = NET_ANY_PORT) or
    (ANetScope.Port = APort);
end;

{ A port-scoped deny covers its port, and any request whose port is not
  known (zero). }
function NetDenyPortMatches(const ANetScope: TGocciaNetScope;
  const APort: Integer): Boolean;
begin
  Result := (ANetScope.Port = 0) or (APort <= 0) or
    (APort = NET_ANY_PORT) or (ANetScope.Port = APort);
end;

function NetRuleDenies(const ARule: TGocciaCapabilityRule;
  const ARequest: TGocciaNetRequest): Boolean;
var
  I: Integer;
  NetScope: TGocciaNetScope;
begin
  if ARule.DenyAll then
    Exit(True);
  for I := 0 to High(ARule.DenyScopes) do
    if TryParseNetScope(ARule.DenyScopes[I], NetScope) then
    begin
      if NetScope.Kind = nskPrivate then
      begin
        if ARequest.HasAddress and ARequest.AddressIsPrivate then
          Exit(True);
      end
      else if NetScopeMatchesHost(NetScope, ARequest.Host,
        ARequest.HostIsAddress, ARequest.HostAddress, ARequest.Port,
        True) then
        Exit(True)
      else if ARequest.HasAddress and NetDenyPortMatches(NetScope,
        ARequest.Port) and NetScopeCoversAddress(NetScope,
        ARequest.Address, True) then
        Exit(True);
    end;
  Result := False;
end;

{ Whether one layer allows the request. A host allow (unscoped, host,
  wildcard, or an IP/CIDR matching an IP-literal host) reaches public
  destinations. `private` reaches private destinations on its own. A private
  destination reached through a host allow also needs `private` or an IP/CIDR
  scope covering the address to be named. Before resolution a host name that
  only `private` could allow passes provisionally. }
function NetRuleAllows(const ARule: TGocciaCapabilityRule;
  const ARequest: TGocciaNetRequest): Boolean;
var
  I: Integer;
  HostMatches, HasPrivate, NamesAddress: Boolean;
  NetScope: TGocciaNetScope;
begin
  HostMatches := ARule.AllowAll;
  HasPrivate := False;
  NamesAddress := False;
  for I := 0 to High(ARule.AllowScopes) do
    if TryParseNetScope(ARule.AllowScopes[I], NetScope) then
    begin
      if NetScope.Kind = nskPrivate then
        HasPrivate := True
      else
      begin
        if (not HostMatches) and NetScopeMatchesHost(NetScope, ARequest.Host,
           ARequest.HostIsAddress, ARequest.HostAddress, ARequest.Port,
           False) then
          HostMatches := True;
        if ARequest.HasAddress and (not NamesAddress) and
           NetScopePortMatches(NetScope, ARequest.Port) and
           NetScopeCoversAddress(NetScope, ARequest.Address, False) then
          NamesAddress := True;
      end;
    end;

  if not ARequest.HasAddress then
    Result := HostMatches or HasPrivate
  else if ARequest.AddressIsPrivate then
    Result := HasPrivate or (HostMatches and NamesAddress)
  else
    Result := HostMatches;
end;

function NetRequestVerdict(const ALayers: TGocciaCapabilityLayers;
  const ARequest: TGocciaNetRequest): TGocciaNetHostVerdict;
var
  I: Integer;
  PublicRequest: TGocciaNetRequest;
begin
  if Length(ALayers) = 0 then
    Exit(nhvNotAllowed);
  for I := 0 to High(ALayers) do
    if NetRuleDenies(ALayers[I].Rules[gcNet], ARequest) then
      Exit(nhvDenied);
  for I := 0 to High(ALayers) do
    if not NetRuleAllows(ALayers[I].Rules[gcNet], ARequest) then
    begin
      { A private destination the layer would allow were it public was
        refused only because nothing names the private address. }
      PublicRequest := ARequest;
      PublicRequest.AddressIsPrivate := False;
      if ARequest.HasAddress and ARequest.AddressIsPrivate and
         NetRuleAllows(ALayers[I].Rules[gcNet], PublicRequest) then
        Exit(nhvPrivateNotNamed);
      Exit(nhvNotAllowed);
    end;
  Result := nhvAllowed;
end;

function TryBuildNetRequest(const AHost: string; const APort: Integer;
  out ARequest: TGocciaNetRequest): Boolean;
begin
  ARequest := Default(TGocciaNetRequest);
  ARequest.Host := NormalizeRequestHost(AHost);
  ARequest.Port := APort;
  if ARequest.Host = '' then
    Exit(False);
  ARequest.HostIsAddress := TryParseIPAddress(ARequest.Host,
    ARequest.HostAddress);
  if ARequest.HostIsAddress then
  begin
    ARequest.HasAddress := True;
    ARequest.Address := ARequest.HostAddress;
    ARequest.AddressIsPrivate := IsPrivateIPAddress(ARequest.Address);
  end;
  Result := True;
end;

function TGocciaCapabilities.NetHostVerdict(const AHost: string;
  const APort: Integer): TGocciaNetHostVerdict;
var
  Request: TGocciaNetRequest;
begin
  if not TryBuildNetRequest(AHost, APort, Request) then
    Exit(nhvNotAllowed);
  Result := NetRequestVerdict(FLayers, Request);
end;

function TGocciaCapabilities.AllowsNetHost(const AHost: string;
  const APort: Integer): Boolean;
begin
  Result := NetHostVerdict(AHost, APort) = nhvAllowed;
end;

function TGocciaCapabilities.NetDenyScope(const AHost: string;
  const APort: Integer; out AScope: string): Boolean;
var
  Request: TGocciaNetRequest;
  Single: TGocciaCapabilityRule;
  I, J: Integer;
begin
  AScope := '';
  if not TryBuildNetRequest(AHost, APort, Request) then
    Exit(False);
  for I := 0 to High(FLayers) do
  begin
    if FLayers[I].Rules[gcNet].DenyAll then
      Exit(True);
    for J := 0 to High(FLayers[I].Rules[gcNet].DenyScopes) do
    begin
      Single := Default(TGocciaCapabilityRule);
      SetLength(Single.DenyScopes, 1);
      Single.DenyScopes[0] := FLayers[I].Rules[gcNet].DenyScopes[J];
      if NetRuleDenies(Single, Request) then
      begin
        AScope := Single.DenyScopes[0];
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

function TGocciaCapabilities.ExplainNetHostDenial(const AHost: string;
  const APort: Integer): string;
begin
  case NetHostVerdict(AHost, APort) of
    nhvAllowed:
      Result := '';
    nhvDenied:
      Result := 'a net deny covers this host';
    nhvPrivateNotNamed:
      Result := 'the host is a private, loopback, or link-local address ' +
        'the net capability does not name';
  else
    if NetHostVerdict(AHost, NET_ANY_PORT) = nhvAllowed then
      Result := Format('the net capability does not allow port %d of this ' +
        'host', [APort])
    else
      Result := 'the net capability does not allow this host';
  end;
end;

function TGocciaCapabilities.AllowsNetAddress(const AHost: string;
  const APort: Integer; const AAddress: string): Boolean;
var
  Request: TGocciaNetRequest;
begin
  if not TryBuildNetRequest(AHost, APort, Request) then
    Exit(False);
  { An unparseable resolution result is refused rather than classified. }
  if not TryParseIPAddress(NormalizeRequestHost(AAddress), Request.Address) then
    Exit(False);
  Request.HasAddress := True;
  Request.AddressIsPrivate := IsPrivateIPAddress(Request.Address);
  Result := NetRequestVerdict(FLayers, Request) = nhvAllowed;
end;

{ Whether ADirectory lies inside the node_modules ceiling ACeiling, and the
  ceiling spelled the way the ancestor walk will compare it. Both are
  expanded spellings, and the walk compares expanded spellings, so the
  expanded ceiling is used when the spellings agree. They can disagree while
  naming the same place — macOS's /var is /private/var, and the working
  directory comes back physical — so containment is also asked of the
  canonical paths; the ceiling is then handed on canonically, which is how a
  physically spelled importer is compared. }
function DirectoryWithinCeiling(const ADirectory, ACeiling: string;
  out AWalkCeiling: string): Boolean;
var
  CanonicalCeiling: string;
begin
  AWalkCeiling := ACeiling;
  if IsPathWithinScope(ADirectory, ACeiling) then
    Exit(True);
  CanonicalCeiling := CanonicalCapabilityPath(ACeiling);
  Result := (CanonicalCeiling <> '') and IsPathWithinScope(
    CanonicalCapabilityPath(ADirectory), CanonicalCeiling);
  if Result then
    AWalkCeiling := CanonicalCeiling;
end;

function TGocciaCapabilities.NodeModulesCeiling(
  const AImportingDirectory: string; out ACeiling: string): Boolean;
var
  WalkCeiling: string;
  Directory, LayerCeiling: string;
  I, J: Integer;
  ImportScope: TGocciaImportScope;
  LayerAllows, LayerUnbounded: Boolean;
  Rule: TGocciaCapabilityRule;
begin
  ACeiling := '';
  if (Length(FLayers) = 0) or DeniesNodeModules(AImportingDirectory) then
    Exit(False);
  if AImportingDirectory <> '' then
    Directory := StripTrailingDelimiter(
      ExpandHostFileName(AImportingDirectory))
  else
    Directory := '';

  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[gcImport];
    LayerAllows := Rule.AllowAll;
    LayerUnbounded := Rule.AllowAll;
    LayerCeiling := '';
    for J := 0 to High(Rule.AllowScopes) do
      if TryParseImportScope(Rule.AllowScopes[J], ImportScope) and
         (ImportScope.Kind = iskNodeModules) then
      begin
        if ImportScope.Ceiling = '' then
        begin
          LayerAllows := True;
          LayerUnbounded := True;
        end
        else if DirectoryWithinCeiling(Directory, ImportScope.Ceiling,
          WalkCeiling) then
        begin
          LayerAllows := True;
          { Within one layer the grants are a union: the highest ceiling that
            still contains the importer is the most the layer allows. }
          if (LayerCeiling = '') or
             (Length(WalkCeiling) < Length(LayerCeiling)) then
            LayerCeiling := WalkCeiling;
        end;
      end;
    if not LayerAllows then
      Exit(False);
    { Every layer must allow the walk, so across layers the deepest ceiling
      wins. All candidate ceilings contain the importer, so they nest. }
    if (not LayerUnbounded) and (Length(LayerCeiling) > Length(ACeiling)) then
      ACeiling := LayerCeiling;
  end;
  Result := True;
end;

function TGocciaCapabilities.DeniesNodeModules(
  const AImportingDirectory: string): Boolean;
var
  Directory, WalkCeiling: string;
  I, J: Integer;
  ImportScope: TGocciaImportScope;
  Rule: TGocciaCapabilityRule;
begin
  if AImportingDirectory <> '' then
    Directory := StripTrailingDelimiter(
      ExpandHostFileName(AImportingDirectory))
  else
    Directory := '';
  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[gcImport];
    if Rule.DenyAll then
      Exit(True);
    for J := 0 to High(Rule.DenyScopes) do
      if TryParseImportScope(Rule.DenyScopes[J], ImportScope) and
         (ImportScope.Kind = iskNodeModules) and
         ((ImportScope.Ceiling = '') or
          DirectoryWithinCeiling(Directory, ImportScope.Ceiling,
            WalkCeiling)) then
        Exit(True);
  end;
  Result := False;
end;

function TGocciaCapabilities.AllowsProvider(const AProvider: string): Boolean;
var
  Provider: string;
  I, J: Integer;
  ImportScope: TGocciaImportScope;
  LayerAllows: Boolean;
  Rule: TGocciaCapabilityRule;
begin
  if Length(FLayers) = 0 then
    Exit(False);
  Provider := LowerCase(Trim(AProvider));
  if Provider = '' then
    Exit(False);
  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[gcImport];
    if Rule.DenyAll then
      Exit(False);
    for J := 0 to High(Rule.DenyScopes) do
      if TryParseImportScope(Rule.DenyScopes[J], ImportScope) and
         (ImportScope.Kind = iskProvider) and
         (ImportScope.Provider = Provider) then
        Exit(False);
  end;
  for I := 0 to High(FLayers) do
  begin
    Rule := FLayers[I].Rules[gcImport];
    LayerAllows := Rule.AllowAll;
    J := 0;
    while (not LayerAllows) and (J <= High(Rule.AllowScopes)) do
    begin
      LayerAllows := TryParseImportScope(Rule.AllowScopes[J], ImportScope) and
        (ImportScope.Kind = iskProvider) and
        (ImportScope.Provider = Provider);
      Inc(J);
    end;
    if not LayerAllows then
      Exit(False);
  end;
  Result := True;
end;

function TGocciaCapabilities.Allows(const ACapability: TGocciaCapability;
  const ARequest: string): Boolean;
var
  NetScope: TGocciaNetScope;
  Host, Ceiling: string;
  CloseBracket: Integer;
begin
  case ACapability of
    gcRead, gcFFI:
      Result := AllowsPath(ACapability, ARequest);
    gcNet:
      begin
        if (not TryParseNetScope(ARequest, NetScope)) or
           not (NetScope.Kind in [nskHost, nskAddress]) then
          Exit(False);
        Host := LowerCase(Trim(ARequest));
        if NetScope.Kind = nskHost then
          Host := NetScope.Host
        else if Host[1] = '[' then
        begin
          CloseBracket := Pos(']', Host);
          Host := Copy(Host, 2, CloseBracket - 2);
        end
        else if (NetScope.Port <> 0) and (Pos(':', Host) > 0) then
          Host := Copy(Host, 1, Pos(':', Host) - 1);
        Result := AllowsNetHost(Host, NetScope.Port);
      end;
    gcImport:
      if SameText(Trim(ARequest), IMPORT_NODE_MODULES_SCOPE) then
        Result := NodeModulesCeiling('', Ceiling)
      else
        Result := AllowsProvider(ARequest);
  else
    Result := False;
  end;
end;

function ScopesToJSON(const AScopes: TGocciaCapabilityScopes): string;
var
  I: Integer;
begin
  Result := '[';
  for I := 0 to High(AScopes) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + QuoteJSONString(AScopes[I]);
  end;
  Result := Result + ']';
end;

function BooleanToJSON(const AValue: Boolean): string;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TGocciaCapabilities.ToJSON: string;
var
  I: Integer;
  Capability: TGocciaCapability;
  Rule: TGocciaCapabilityRule;
begin
  Result := '{"layers":[';
  for I := 0 to High(FLayers) do
  begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + '{';
    for Capability := Low(TGocciaCapability) to High(TGocciaCapability) do
    begin
      if Capability <> Low(TGocciaCapability) then
        Result := Result + ',';
      Rule := FLayers[I].Rules[Capability];
      Result := Result + QuoteJSONString(CapabilityName(Capability)) +
        ':{"allowAll":' + BooleanToJSON(Rule.AllowAll) +
        ',"allow":' + ScopesToJSON(Rule.AllowScopes) +
        ',"denyAll":' + BooleanToJSON(Rule.DenyAll) +
        ',"deny":' + ScopesToJSON(Rule.DenyScopes) + '}';
    end;
    Result := Result + '}';
  end;
  Result := Result + ']}';
end;

end.
