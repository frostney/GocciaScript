program Goccia.Capabilities.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.TestSetup;

type
  TCapabilitiesTests = class(TTestSuite)
  private
    function RootPath(const ARelative: string): string;
    procedure TestNoneGrantsNothing;
    procedure TestZeroValueGrantsNothing;
    procedure TestUnrestrictedGrantsEverything;
    procedure TestDenyWinsAllowFirst;
    procedure TestDenyWinsDenyFirst;
    procedure TestDenyAllBeatsScopedAllow;
    procedure TestPathSubtreeCarveOut;
    procedure TestPathBoundary;
    procedure TestRelativeScopesRejected;
    procedure TestMalformedNetScopesRejected;
    procedure TestSymlinkCanonicalization;
    procedure TestNetHostAndPort;
    procedure TestNetWildcard;
    procedure TestNetCIDR;
    procedure TestNetIPv6;
    procedure TestPrivateMustBeNamed;
    procedure TestPrivateIsAStandaloneGrant;
    procedure TestExplicitAddressNamesPrivate;
    procedure TestDenyPrivateWins;
    procedure TestNarrowingNeverWidens;
    procedure TestNarrowToNoneDeniesAll;
    procedure TestImmutabilityAndCopyIsolation;
    procedure TestNodeModulesCeilings;
    procedure TestNodeModulesDeny;
    procedure TestProviderScopes;
    procedure TestGrantsAndDeniesAll;
    procedure TestAllowsUnscoped;
    procedure TestNetTrailingDot;
    procedure TestNetMappedIPv6;
    procedure TestNetEmbeddedIPv4Ranges;
    procedure TestNetEmbeddedIPv4Scopes;
    procedure TestNetIPLiteralTrailingDot;
    procedure TestNetCIDRZero;
    procedure TestExplainNetHostDenial;
    procedure TestToJSON;
  public
    procedure SetupTests; override;
  end;

procedure TCapabilitiesTests.SetupTests;
begin
  Test('None grants nothing', TestNoneGrantsNothing);
  Test('A zero-initialized set grants nothing', TestZeroValueGrantsNothing);
  Test('Unrestricted grants every capability', TestUnrestrictedGrantsEverything);
  Test('Deny wins when the allow came first', TestDenyWinsAllowFirst);
  Test('Deny wins when the deny came first', TestDenyWinsDenyFirst);
  Test('Deny-all beats a scoped allow', TestDenyAllBeatsScopedAllow);
  Test('A deny scope carves a subtree out of an allow',
    TestPathSubtreeCarveOut);
  Test('Path scopes stop at a separator boundary', TestPathBoundary);
  Test('Relative path scopes are rejected', TestRelativeScopesRejected);
  Test('Malformed net scopes are rejected', TestMalformedNetScopesRejected);
  Test('Path scopes and requests are symlink-canonicalized',
    TestSymlinkCanonicalization);
  Test('net host scopes match host and optional port', TestNetHostAndPort);
  Test('net wildcard scopes match subdomains only', TestNetWildcard);
  Test('net CIDR scopes match addresses in range', TestNetCIDR);
  Test('net IPv6 literals and ranges', TestNetIPv6);
  Test('Private ranges must be named', TestPrivateMustBeNamed);
  Test('private grants private destinations on its own',
    TestPrivateIsAStandaloneGrant);
  Test('An explicit IP or CIDR names a private address',
    TestExplicitAddressNamesPrivate);
  Test('Deny private wins over explicit addresses', TestDenyPrivateWins);
  Test('Narrowing never widens', TestNarrowingNeverWidens);
  Test('Narrowing to None denies everything', TestNarrowToNoneDeniesAll);
  Test('Builders never mutate their receiver',
    TestImmutabilityAndCopyIsolation);
  Test('node_modules ceilings bound the ancestor walk',
    TestNodeModulesCeilings);
  Test('node_modules denies win', TestNodeModulesDeny);
  Test('Provider import scopes', TestProviderScopes);
  Test('Grants and DeniesAll', TestGrantsAndDeniesAll);
  Test('AllowsUnscoped needs an unscoped allow and no deny in every layer',
    TestAllowsUnscoped);
  Test('A trailing dot names the same host', TestNetTrailingDot);
  Test('IPv4-mapped IPv6 literals are judged as IPv4', TestNetMappedIPv6);
  Test('NAT64, 6to4, and IPv4-compatible forms of private addresses stay ' +
    'private', TestNetEmbeddedIPv4Ranges);
  Test('IP and CIDR denies match the IPv4 host a NAT64 or 6to4 address ' +
    'embeds; allows do not', TestNetEmbeddedIPv4Scopes);
  Test('A trailing dot on an IP-literal scope names the same address',
    TestNetIPLiteralTrailingDot);
  Test('A /0 range covers every address, private ones included',
    TestNetCIDRZero);
  Test('ExplainNetHostDenial names the reason a host is refused',
    TestExplainNetHostDenial);
  Test('ToJSON serializes every layer', TestToJSON);
end;

function TCapabilitiesTests.RootPath(const ARelative: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := 'C:\capability-test\' + StringReplace(ARelative, '/', '\',
    [rfReplaceAll]);
  {$ELSE}
  Result := '/capability-test/' + ARelative;
  {$ENDIF}
end;

procedure TCapabilitiesTests.TestNoneGrantsNothing;
var
  Capabilities: TGocciaCapabilities;
  Ceiling: string;
begin
  Capabilities := TGocciaCapabilities.None;
  Expect<Boolean>(Capabilities.Grants(gcRead)).ToBe(False);
  Expect<Boolean>(Capabilities.Grants(gcNet)).ToBe(False);
  Expect<Boolean>(Capabilities.Grants(gcFFI)).ToBe(False);
  Expect<Boolean>(Capabilities.Grants(gcImport)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead, RootPath('a'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(False);
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('a'),
    Ceiling)).ToBe(False);
  Expect<Boolean>(Capabilities.DeniesAll(gcRead)).ToBe(False);
end;

procedure TCapabilitiesTests.TestZeroValueGrantsNothing;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := Default(TGocciaCapabilities);
  Expect<Integer>(Capabilities.LayerCount).ToBe(0);
  Expect<Boolean>(Capabilities.Grants(gcRead)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead, RootPath('a'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 80)).ToBe(False);
  { Allow on a zero value creates the root layer. }
  Capabilities := Capabilities.Allow(gcNet, 'example.com');
  Expect<Integer>(Capabilities.LayerCount).ToBe(1);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 80)).ToBe(True);
end;

procedure TCapabilitiesTests.TestUnrestrictedGrantsEverything;
var
  Capabilities: TGocciaCapabilities;
  Ceiling: string;
begin
  Capabilities := TGocciaCapabilities.Unrestricted;
  Expect<Boolean>(Capabilities.AllowsPath(gcRead, RootPath('a'))).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsPath(gcFFI, RootPath('lib.so'))).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('intranet.test', 80,
    '10.0.0.1')).ToBe(True);
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('a'),
    Ceiling)).ToBe(True);
  Expect<string>(Ceiling).ToBe('');
  Expect<Boolean>(Capabilities.AllowsProvider('github')).ToBe(True);
end;

procedure TCapabilitiesTests.TestDenyWinsAllowFirst;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None
    .Allow(gcNet, 'example.com')
    .Deny(gcNet, 'example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(False);
  Capabilities := TGocciaCapabilities.None
    .Allow(gcRead, RootPath('data'))
    .Deny(gcRead, RootPath('data'));
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('data/x.txt'))).ToBe(False);
end;

procedure TCapabilitiesTests.TestDenyWinsDenyFirst;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None
    .Deny(gcNet, 'example.com')
    .Allow(gcNet, 'example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(False);
  Capabilities := TGocciaCapabilities.None
    .Deny(gcRead, RootPath('data'))
    .Allow(gcRead, RootPath('data'));
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('data/x.txt'))).ToBe(False);
end;

procedure TCapabilitiesTests.TestDenyAllBeatsScopedAllow;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None
    .Allow(gcRead, RootPath('data'))
    .Deny(gcRead);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('data/x.txt'))).ToBe(False);
  Expect<Boolean>(Capabilities.DeniesAll(gcRead)).ToBe(True);
  Expect<Boolean>(Capabilities.Grants(gcRead)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Deny(gcNet)
    .Allow(gcNet, 'example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(False);
end;

procedure TCapabilitiesTests.TestPathSubtreeCarveOut;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None
    .Allow(gcRead, RootPath('project'))
    .Deny(gcRead, RootPath('project/secrets'));
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('project/src/app.js'))).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('project/secrets'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('project/secrets/key.pem'))).ToBe(False);
  Expect<Boolean>(Capabilities.DeniesPath(gcRead,
    RootPath('project/secrets/key.pem'))).ToBe(True);
  Expect<Boolean>(Capabilities.DeniesPath(gcRead,
    RootPath('project/src/app.js'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('other/file'))).ToBe(False);
end;

procedure TCapabilitiesTests.TestPathBoundary;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcRead, RootPath('a/b'));
  Expect<Boolean>(Capabilities.AllowsPath(gcRead, RootPath('a/b'))).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('a/b/c.js'))).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('a/bc'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('a/bc/d.js'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead, RootPath('a'))).ToBe(False);
  { A trailing separator on the scope is the same scope. }
  Capabilities := TGocciaCapabilities.None.Allow(gcRead,
    RootPath('a/b') + PathDelim);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('a/bc'))).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('a/b/x'))).ToBe(True);
  { `..` in a request cannot climb out of the scope. }
  Expect<Boolean>(Capabilities.AllowsPath(gcRead,
    RootPath('a/b/../bc/x'))).ToBe(False);
  { The filesystem root covers everything. }
  Capabilities := TGocciaCapabilities.None.Allow(gcRead, PathDelim);
  Expect<Boolean>(Capabilities.AllowsPath(gcRead, RootPath('z'))).ToBe(True);
end;

procedure TCapabilitiesTests.TestRelativeScopesRejected;
var
  Raised: Boolean;
begin
  Raised := False;
  try
    TGocciaCapabilities.None.Allow(gcRead, 'relative/dir');
  except
    on E: EGocciaCapabilityScopeError do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);

  Raised := False;
  try
    TGocciaCapabilities.None.Deny(gcFFI, './lib.so');
  except
    on E: EGocciaCapabilityScopeError do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);

  Raised := False;
  try
    TGocciaCapabilities.None.Allow(gcImport, 'node_modules=relative');
  except
    on E: EGocciaCapabilityScopeError do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TCapabilitiesTests.TestMalformedNetScopesRejected;
const
  MALFORMED: array[0..6] of string = ('exa mple.com', 'example.com:0',
    'example.com:70000', '10.0.0.0/33', 'http://example.com', 'a..b',
    '[::1');
var
  I: Integer;
  Raised: Boolean;
begin
  for I := Low(MALFORMED) to High(MALFORMED) do
  begin
    Raised := False;
    try
      TGocciaCapabilities.None.Allow(gcNet, MALFORMED[I]);
    except
      on E: EGocciaCapabilityScopeError do
        Raised := True;
    end;
    if not Raised then
      Fail('Expected malformed net scope to be rejected: ' + MALFORMED[I]);
  end;
  Expect<Boolean>(True).ToBe(True);
end;

procedure TCapabilitiesTests.TestSymlinkCanonicalization;
{$IFDEF UNIX}
var
  Base, RealDirectory, LinkDirectory, Target: string;
  Capabilities: TGocciaCapabilities;
{$ENDIF}
begin
  {$IFDEF UNIX}
  Base := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'goccia-capabilities-' + IntToStr(GetProcessID);
  RealDirectory := Base + '/real';
  LinkDirectory := Base + '/link';
  ForceDirectories(RealDirectory + '/inner');
  try
    Target := RealDirectory;
    if fpSymlink(PAnsiChar(AnsiString(Target)),
       PAnsiChar(AnsiString(LinkDirectory))) <> 0 then
      Fail('could not create the test symlink');

    { A scope spelled through the link covers the real directory. }
    Capabilities := TGocciaCapabilities.None.Allow(gcRead, LinkDirectory);
    Expect<Boolean>(Capabilities.AllowsPath(gcRead,
      RealDirectory + '/inner')).ToBe(True);

    { A request spelled through the link is judged by where it resolves. }
    Capabilities := TGocciaCapabilities.None.Allow(gcRead, RealDirectory);
    Expect<Boolean>(Capabilities.AllowsPath(gcRead,
      LinkDirectory + '/inner')).ToBe(True);
    Expect<Boolean>(Capabilities.AllowsPath(gcRead,
      LinkDirectory + '/not-yet-created.js')).ToBe(True);

    { A deny on the real path cannot be dodged through the link. }
    Capabilities := TGocciaCapabilities.None.Allow(gcRead, Base)
      .Deny(gcRead, RealDirectory);
    Expect<Boolean>(Capabilities.AllowsPath(gcRead,
      LinkDirectory + '/inner')).ToBe(False);
  finally
    DeleteFile(LinkDirectory);
    RemoveDir(RealDirectory + '/inner');
    RemoveDir(RealDirectory);
    RemoveDir(Base);
  end;
  {$ELSE}
  Expect<Boolean>(True).ToBe(True);
  {$ENDIF}
end;

procedure TCapabilitiesTests.TestNetHostAndPort;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None
    .Allow(gcNet, 'api.example.com')
    .Allow(gcNet, 'files.example.com:8443');
  Expect<Boolean>(Capabilities.AllowsNetHost('api.example.com', 443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('API.Example.com', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('files.example.com',
    8443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('files.example.com',
    443)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('evil-api.example.com',
    443)).ToBe(False);
  Expect<Boolean>(Capabilities.Allows(gcNet, 'files.example.com:8443'))
    .ToBe(True);
  Expect<Boolean>(Capabilities.Allows(gcNet, 'files.example.com'))
    .ToBe(False);
  { An unscoped allow reaches any public host. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet);
  Expect<Boolean>(Capabilities.AllowsNetHost('anything.test', 80)).ToBe(True);
end;

procedure TCapabilitiesTests.TestNetWildcard;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '*.example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('a.example.com', 443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('a.b.example.com',
    443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 443)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('badexample.com',
    443)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com.evil.test',
    443)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '*.example.com:8080');
  Expect<Boolean>(Capabilities.AllowsNetHost('a.example.com', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('a.example.com', 80)).ToBe(False);
  { A carve-out inside a wildcard. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '*.example.com')
    .Deny(gcNet, 'admin.example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('admin.example.com',
    443)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('www.example.com',
    443)).ToBe(True);
end;

procedure TCapabilitiesTests.TestNetCIDR;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '203.0.113.0/24');
  Expect<Boolean>(Capabilities.AllowsNetHost('203.0.113.7', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('203.0.114.7', 80)).ToBe(False);
  { An IP scope is not a hostname scope: names are not resolved to match. }
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 80)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Deny(gcNet, '198.51.100.0/24');
  Expect<Boolean>(Capabilities.AllowsNetHost('198.51.100.9', 80)).ToBe(False);
  { A CIDR deny also applies to where a hostname resolved. }
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '198.51.100.9')).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '203.0.113.9')).ToBe(True);
end;

procedure TCapabilitiesTests.TestNetIPv6;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '2001:db8::/32');
  Expect<Boolean>(Capabilities.AllowsNetHost('2001:db8::1', 443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('[2001:db8::1]', 443)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('2001:db9::1', 443)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '[2001:db8::5]:8080');
  Expect<Boolean>(Capabilities.AllowsNetHost('2001:db8:0::5', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('2001:db8::5', 80)).ToBe(False);
end;

procedure TCapabilitiesTests.TestPrivateMustBeNamed;
var
  Capabilities: TGocciaCapabilities;
begin
  { An unscoped allow does not imply private ranges. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 80)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('169.254.169.254', 80))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '10.1.2.3')).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '93.184.216.34')).ToBe(True);

  { A hostname allow whose resolution lands in a private range is refused. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, 'intranet.example');
  Expect<Boolean>(Capabilities.AllowsNetHost('intranet.example', 80))
    .ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('intranet.example', 80,
    '10.0.0.5')).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('intranet.example', 80,
    '::1')).ToBe(False);

  { Naming `private` lifts the refusal for otherwise allowed destinations. }
  Capabilities := Capabilities.Allow(gcNet, NET_PRIVATE_SCOPE);
  Expect<Boolean>(Capabilities.AllowsNetAddress('intranet.example', 80,
    '10.0.0.5')).ToBe(True);
end;

procedure TCapabilitiesTests.TestPrivateIsAStandaloneGrant;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, NET_PRIVATE_SCOPE);
  Expect<Boolean>(Capabilities.Grants(gcNet)).ToBe(True);
  { Private, loopback, and link-local literals are granted by `private`. }
  Expect<Boolean>(Capabilities.AllowsNetHost('10.0.0.5', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('[::1]', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('169.254.169.254', 80))
    .ToBe(True);
  { Public literals are not. }
  Expect<Boolean>(Capabilities.AllowsNetHost('93.184.216.34', 80))
    .ToBe(False);
  { A name passes provisionally; where it resolves decides. }
  Expect<Boolean>(Capabilities.AllowsNetHost('localhost', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('localhost', 8080,
    '127.0.0.1')).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 443,
    '93.184.216.34')).ToBe(False);

  { With a host allow beside it, public destinations need the host. }
  Capabilities := Capabilities.Allow(gcNet, 'example.com');
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 443,
    '93.184.216.34')).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('other.test', 443,
    '93.184.216.35')).ToBe(False);

  { Every layer must grant the private destination. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, NET_PRIVATE_SCOPE)
    .Narrow(TGocciaCapabilities.None.Allow(gcNet, 'localhost'));
  Expect<Boolean>(Capabilities.AllowsNetAddress('localhost', 80,
    '127.0.0.1')).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, NET_PRIVATE_SCOPE)
    .Narrow(TGocciaCapabilities.None.Allow(gcNet, NET_PRIVATE_SCOPE));
  Expect<Boolean>(Capabilities.AllowsNetAddress('localhost', 80,
    '127.0.0.1')).ToBe(True);
end;

procedure TCapabilitiesTests.TestExplicitAddressNamesPrivate;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('127.0.0.1', 8080,
    '127.0.0.1')).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('127.0.0.1', 8080,
    '127.0.0.2')).ToBe(False);
  { An IP scope does not match a name: localhost needs a host allow. }
  Expect<Boolean>(Capabilities.AllowsNetHost('localhost', 8080)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '10.0.0.0/8');
  Expect<Boolean>(Capabilities.AllowsNetHost('10.20.30.40', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('192.168.0.1', 80)).ToBe(False);
  { A port-scoped address names the address for that port only. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1:8080');
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 8081)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1')
    .Deny(gcNet, '127.0.0.1:9000');
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 8080)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 9000)).ToBe(False);
end;

procedure TCapabilitiesTests.TestDenyPrivateWins;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1')
    .Allow(gcNet, NET_PRIVATE_SCOPE)
    .Deny(gcNet, NET_PRIVATE_SCOPE);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 80)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('127.0.0.1', 80,
    '127.0.0.1')).ToBe(False);
  Capabilities := Capabilities.Allow(gcNet, 'example.com');
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '93.184.216.34')).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '10.0.0.1')).ToBe(False);
end;

procedure TCapabilitiesTests.TestNarrowingNeverWidens;
const
  SCOPES: array[0..4] of string = ('', 'a', 'a/b', 'c', 'a/b/c');
  REQUESTS: array[0..4] of string = ('a/x', 'a/b/x', 'a/b/c/x', 'c/x', 'd/x');
var
  Parent, Child, Narrowed: TGocciaCapabilities;
  P, C, R, Mode: Integer;

  function Build(const AScopeIndex, AMode: Integer): TGocciaCapabilities;
  begin
    Result := TGocciaCapabilities.None;
    if SCOPES[AScopeIndex] = '' then
      Result := Result.Allow(gcRead)
    else
      Result := Result.Allow(gcRead, RootPath(SCOPES[AScopeIndex]));
    if (AMode = 1) and (SCOPES[AScopeIndex] <> '') then
      Result := Result.Deny(gcRead, RootPath(SCOPES[AScopeIndex] + '/c'));
  end;

begin
  for Mode := 0 to 1 do
    for P := Low(SCOPES) to High(SCOPES) do
      for C := Low(SCOPES) to High(SCOPES) do
      begin
        Parent := Build(P, Mode);
        Child := Build(C, 1 - Mode);
        Narrowed := Parent.Narrow(Child);
        for R := Low(REQUESTS) to High(REQUESTS) do
          if Narrowed.AllowsPath(gcRead, RootPath(REQUESTS[R])) and
             not (Parent.AllowsPath(gcRead, RootPath(REQUESTS[R])) and
               Child.AllowsPath(gcRead, RootPath(REQUESTS[R]))) then
            Fail(Format('narrowing widened %s (parent %s, child %s)',
              [REQUESTS[R], SCOPES[P], SCOPES[C]]));
      end;

  Parent := TGocciaCapabilities.None.Allow(gcNet, 'example.com');
  Narrowed := Parent.Narrow(TGocciaCapabilities.Unrestricted);
  Expect<Boolean>(Narrowed.AllowsNetHost('example.com', 443)).ToBe(True);
  Expect<Boolean>(Narrowed.AllowsNetHost('other.test', 443)).ToBe(False);
  Expect<Boolean>(Narrowed.AllowsNetAddress('example.com', 443,
    '127.0.0.1')).ToBe(False);
  Expect<Boolean>(Narrowed.Grants(gcFFI)).ToBe(False);
  Expect<Integer>(Narrowed.LayerCount).ToBe(2);
  { An allow added after narrowing lands in the child layer only. }
  Narrowed := Narrowed.Allow(gcNet, 'other.test');
  Expect<Boolean>(Narrowed.AllowsNetHost('other.test', 443)).ToBe(False);
end;

procedure TCapabilitiesTests.TestNarrowToNoneDeniesAll;
var
  Narrowed: TGocciaCapabilities;
begin
  Narrowed := TGocciaCapabilities.Unrestricted.Narrow(
    TGocciaCapabilities.None);
  Expect<Boolean>(Narrowed.AllowsPath(gcRead, RootPath('a'))).ToBe(False);
  Narrowed := TGocciaCapabilities.Unrestricted.Narrow(
    Default(TGocciaCapabilities));
  Expect<Boolean>(Narrowed.AllowsNetHost('example.com', 443)).ToBe(False);
end;

procedure TCapabilitiesTests.TestImmutabilityAndCopyIsolation;
var
  Base, Extended, Copied, Denied: TGocciaCapabilities;
begin
  Base := TGocciaCapabilities.None.Allow(gcRead, RootPath('a'));
  Extended := Base.Allow(gcRead, RootPath('b'));
  Expect<Boolean>(Base.AllowsPath(gcRead, RootPath('b/x'))).ToBe(False);
  Expect<Boolean>(Extended.AllowsPath(gcRead, RootPath('b/x'))).ToBe(True);

  Copied := Base;
  Denied := Copied.Deny(gcRead, RootPath('a'));
  Expect<Boolean>(Copied.AllowsPath(gcRead, RootPath('a/x'))).ToBe(True);
  Expect<Boolean>(Base.AllowsPath(gcRead, RootPath('a/x'))).ToBe(True);
  Expect<Boolean>(Denied.AllowsPath(gcRead, RootPath('a/x'))).ToBe(False);

  Denied := Extended.Narrow(Base).Deny(gcRead);
  Expect<Boolean>(Extended.AllowsPath(gcRead, RootPath('a/x'))).ToBe(True);
  Expect<Integer>(Extended.LayerCount).ToBe(1);
  Expect<Integer>(Denied.LayerCount).ToBe(2);
end;

procedure TCapabilitiesTests.TestNodeModulesCeilings;
var
  Capabilities: TGocciaCapabilities;
  Ceiling: string;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcImport,
    IMPORT_NODE_MODULES_SCOPE);
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app/src'),
    Ceiling)).ToBe(True);
  Expect<string>(Ceiling).ToBe('');

  Capabilities := TGocciaCapabilities.None.Allow(gcImport,
    IMPORT_NODE_MODULES_SCOPE + '=' + RootPath('app'));
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app/src'),
    Ceiling)).ToBe(True);
  Expect<string>(Ceiling).ToBe(RootPath('app'));
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('elsewhere'),
    Ceiling)).ToBe(False);
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('application'),
    Ceiling)).ToBe(False);

  { Narrowing to a deeper ceiling bounds the walk further. }
  Capabilities := TGocciaCapabilities.None
    .Allow(gcImport, IMPORT_NODE_MODULES_SCOPE)
    .Narrow(TGocciaCapabilities.None.Allow(gcImport,
      IMPORT_NODE_MODULES_SCOPE + '=' + RootPath('app/packages')));
  Expect<Boolean>(Capabilities.NodeModulesCeiling(
    RootPath('app/packages/one'), Ceiling)).ToBe(True);
  Expect<string>(Ceiling).ToBe(RootPath('app/packages'));
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app'),
    Ceiling)).ToBe(False);

  { Two grants in one layer are a union: the higher ceiling wins. }
  Capabilities := TGocciaCapabilities.None
    .Allow(gcImport, IMPORT_NODE_MODULES_SCOPE + '=' + RootPath('app/src'))
    .Allow(gcImport, IMPORT_NODE_MODULES_SCOPE + '=' + RootPath('app'));
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app/src/x'),
    Ceiling)).ToBe(True);
  Expect<string>(Ceiling).ToBe(RootPath('app'));
end;

procedure TCapabilitiesTests.TestNodeModulesDeny;
var
  Capabilities: TGocciaCapabilities;
  Ceiling: string;
begin
  Capabilities := TGocciaCapabilities.None
    .Allow(gcImport, IMPORT_NODE_MODULES_SCOPE)
    .Deny(gcImport, IMPORT_NODE_MODULES_SCOPE + '=' + RootPath('app/vendor'));
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app/src'),
    Ceiling)).ToBe(True);
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app/vendor/x'),
    Ceiling)).ToBe(False);
  Capabilities := Capabilities.Deny(gcImport, IMPORT_NODE_MODULES_SCOPE);
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app/src'),
    Ceiling)).ToBe(False);
end;

procedure TCapabilitiesTests.TestProviderScopes;
var
  Capabilities: TGocciaCapabilities;
  Ceiling: string;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcImport, 'github');
  Expect<Boolean>(Capabilities.AllowsProvider('github')).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsProvider('GitHub')).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsProvider('gitlab')).ToBe(False);
  { A provider grant is not a node_modules grant. }
  Expect<Boolean>(Capabilities.NodeModulesCeiling(RootPath('app'),
    Ceiling)).ToBe(False);
  Capabilities := Capabilities.Deny(gcImport, 'github');
  Expect<Boolean>(Capabilities.AllowsProvider('github')).ToBe(False);
end;

procedure TCapabilitiesTests.TestGrantsAndDeniesAll;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcFFI, RootPath('lib'));
  Expect<Boolean>(Capabilities.Grants(gcFFI)).ToBe(True);
  Expect<Boolean>(Capabilities.Grants(gcRead)).ToBe(False);
  Expect<Boolean>(Capabilities.DeniesAll(gcFFI)).ToBe(False);
  Capabilities := Capabilities.Narrow(TGocciaCapabilities.None);
  Expect<Boolean>(Capabilities.Grants(gcFFI)).ToBe(False);
  Capabilities := TGocciaCapabilities.Unrestricted.Deny(gcRead);
  Expect<Boolean>(Capabilities.DeniesAll(gcRead)).ToBe(True);
  Expect<Boolean>(Capabilities.Grants(gcRead)).ToBe(False);
  Expect<Boolean>(Capabilities.Grants(gcNet)).ToBe(True);
end;

procedure TCapabilitiesTests.TestAllowsUnscoped;
begin
  Expect<Boolean>(TGocciaCapabilities.None.Allow(gcFFI)
    .AllowsUnscoped(gcFFI)).ToBe(True);
  Expect<Boolean>(TGocciaCapabilities.None.Allow(gcFFI, RootPath('lib'))
    .AllowsUnscoped(gcFFI)).ToBe(False);
  Expect<Boolean>(TGocciaCapabilities.None.Allow(gcFFI)
    .Deny(gcFFI, RootPath('lib')).AllowsUnscoped(gcFFI)).ToBe(False);
  Expect<Boolean>(TGocciaCapabilities.None.Allow(gcFFI)
    .Narrow(TGocciaCapabilities.None.Allow(gcFFI, RootPath('lib')))
    .AllowsUnscoped(gcFFI)).ToBe(False);
  Expect<Boolean>(Default(TGocciaCapabilities).AllowsUnscoped(gcFFI))
    .ToBe(False);
end;

procedure TCapabilitiesTests.TestNetTrailingDot;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Deny(gcNet, 'tracker.example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('tracker.example.com.', 443))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('TRACKER.example.com.', 443))
    .ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, 'api.example.com.');
  Expect<Boolean>(Capabilities.AllowsNetHost('api.example.com', 443))
    .ToBe(True);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Deny(gcNet, '*.example.com');
  Expect<Boolean>(Capabilities.AllowsNetHost('a.example.com.', 443))
    .ToBe(False);
end;

procedure TCapabilitiesTests.TestNetMappedIPv6;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Allow(gcNet, NET_PRIVATE_SCOPE).Deny(gcNet, '169.254.169.254');
  Expect<Boolean>(Capabilities.AllowsNetHost('::ffff:169.254.169.254', 80))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('[::ffff:a9fe:a9fe]', 80))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '::ffff:a9fe:a9fe'))
    .ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Deny(gcNet, '198.51.100.0/24');
  Expect<Boolean>(Capabilities.AllowsNetHost('::ffff:198.51.100.7', 80))
    .ToBe(False);
  { A mapped loopback is private, and naming the IPv4 address names it. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet);
  Expect<Boolean>(Capabilities.AllowsNetHost('::ffff:127.0.0.1', 80))
    .ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1');
  Expect<Boolean>(Capabilities.AllowsNetHost('::ffff:7f00:1', 80))
    .ToBe(True);
  { A mapped public address is as public as its IPv4 form. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '::ffff:8.8.8.8'))
    .ToBe(True);
end;

procedure TCapabilitiesTests.TestNetEmbeddedIPv4Ranges;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '::127.0.0.1')).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '64:ff9b::a9fe:a9fe'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '64:ff9b::808:808'))
    .ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '2002:7f00:1::1'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '2002:c0a8:101::'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '2002:808:808::1'))
    .ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('example.com', 80,
    '2606:4700::1111'))
    .ToBe(True);
end;

{ A deny on an IPv4 address must cover every IPv6 spelling that reaches the
  same host, and an allow naming it names them too. }
procedure TCapabilitiesTests.TestNetEmbeddedIPv4Scopes;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Allow(gcNet, NET_PRIVATE_SCOPE).Deny(gcNet, '169.254.169.254');
  Expect<Boolean>(Capabilities.AllowsNetAddress('64:ff9b::a9fe:a9fe', 80,
    '64:ff9b::a9fe:a9fe'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('2002:a9fe:a9fe::1', 80,
    '2002:a9fe:a9fe::1'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('[64:ff9b::a9fe:a9fe]', 80))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('[2002:a9fe:a9fe::1]', 80))
    .ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Deny(gcNet, '198.51.100.0/24');
  Expect<Boolean>(Capabilities.AllowsNetAddress('64:ff9b::c633:6407', 80,
    '64:ff9b::c633:6407'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('2002:c633:6407::', 80,
    '2002:c633:6407::'))
    .ToBe(False);
  { The IPv6 spelling of an address outside the range stays allowed. }
  Expect<Boolean>(Capabilities.AllowsNetAddress('64:ff9b::808:808', 80,
    '64:ff9b::808:808'))
    .ToBe(True);
  { An IPv4 allow does not reach its translated spellings: 2002:a00:5::/48
    names a 6to4 relay site, not the host 10.0.0.5. NAT64 behaves the same. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '10.0.0.5');
  Expect<Boolean>(Capabilities.AllowsNetHost('10.0.0.5', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('[2002:a00:5::1]', 80))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('2002:a00:5::1', 80,
    '2002:a00:5::1'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('64:ff9b::a00:5', 80,
    '64:ff9b::a00:5'))
    .ToBe(False);
  { Nor does an IPv4 range name the private hosts they translate to. }
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Allow(gcNet, '10.0.0.0/8');
  Expect<Boolean>(Capabilities.AllowsNetAddress('64:ff9b::a00:1', 80,
    '64:ff9b::a00:1'))
    .ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('[2002:a00:1::1]', 80))
    .ToBe(False);
end;

procedure TCapabilitiesTests.TestNetIPLiteralTrailingDot;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1.');
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1.', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetAddress('127.0.0.1', 80,
    '127.0.0.1')).ToBe(True);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '127.0.0.1.:18765');
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 18765)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 18766)).ToBe(False);
  Capabilities := TGocciaCapabilities.None.Allow(gcNet)
    .Deny(gcNet, '198.51.100.7.');
  Expect<Boolean>(Capabilities.AllowsNetHost('198.51.100.7', 80)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetAddress('198.51.100.7', 80,
    '198.51.100.7')).ToBe(False);
end;

procedure TCapabilitiesTests.TestNetCIDRZero;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, '0.0.0.0/0');
  Expect<Boolean>(Capabilities.AllowsNetHost('8.8.8.8', 53)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 80)).ToBe(True);
  Expect<Boolean>(Capabilities.AllowsNetHost('169.254.169.254', 80))
    .ToBe(True);
  { Host names are never resolved to match an address scope. }
  Expect<Boolean>(Capabilities.AllowsNetHost('example.com', 80)).ToBe(False);
  { A private deny still wins over the range. }
  Capabilities := Capabilities.Deny(gcNet, NET_PRIVATE_SCOPE);
  Expect<Boolean>(Capabilities.AllowsNetHost('127.0.0.1', 80)).ToBe(False);
  Expect<Boolean>(Capabilities.AllowsNetHost('8.8.8.8', 53)).ToBe(True);
end;

procedure TCapabilitiesTests.TestExplainNetHostDenial;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, 'api.example.com:8443')
    .Deny(gcNet, 'blocked.example.com');
  Expect<string>(Capabilities.ExplainNetHostDenial('api.example.com', 8443))
    .ToBe('');
  Expect<string>(Capabilities.ExplainNetHostDenial('other.example.com', 80))
    .ToBe('the net capability does not allow this host');
  Expect<string>(Capabilities.ExplainNetHostDenial('api.example.com', 443))
    .ToBe('the net capability does not allow port 443 of this host');
  Expect<string>(Capabilities.ExplainNetHostDenial('blocked.example.com', 80))
    .ToBe('a net deny covers this host');
  Capabilities := TGocciaCapabilities.None.Allow(gcNet);
  Expect<string>(Capabilities.ExplainNetHostDenial('10.0.0.1', 80))
    .ToBe('the host is a private, loopback, or link-local address the net ' +
      'capability does not name');
end;

procedure TCapabilitiesTests.TestToJSON;
var
  Capabilities: TGocciaCapabilities;
begin
  Capabilities := TGocciaCapabilities.None.Allow(gcNet, 'Example.com')
    .Deny(gcFFI);
  Expect<string>(Capabilities.ToJSON).ToBe(
    '{"layers":[{' +
    '"read":{"allowAll":false,"allow":[],"denyAll":false,"deny":[]},' +
    '"net":{"allowAll":false,"allow":["example.com"],"denyAll":false,"deny":[]},' +
    '"ffi":{"allowAll":false,"allow":[],"denyAll":true,"deny":[]},' +
    '"import":{"allowAll":false,"allow":[],"denyAll":false,"deny":[]}}]}');
end;

begin
  TestRunnerProgram.AddSuite(TCapabilitiesTests.Create('Capabilities'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
